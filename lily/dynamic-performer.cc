/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "performer.hh"
#include "audio-item.hh"
#include "stream-event.hh"
#include "international.hh"

#include "translator.icc"

using std::vector;

class Dynamic_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Dynamic_performer);

protected:
  void acknowledge_audio_element (Audio_element_info info) override;
  void finalize () override;
  void stop_translation_timestep ();
  void process_music ();
  Real equalize_volume (Real);

  void listen_decrescendo (Stream_event *);
  void listen_crescendo (Stream_event *);
  void listen_absolute_dynamic (Stream_event *);

private:
  void close_and_enqueue_span ();
  Real calc_departure_volume (Direction depart_dir, Real start_vol,
                              Real end_vol, Real min_vol, Real max_vol);
  bool drive_state_machine (Direction next_grow_dir);
  // next_vol < 0 means select a target dynamic based on growth direction.
  // return actual next volume (computed if not provided)
  Real finish_queued_spans (Real next_vol = -1.0);
  Real look_up_absolute_volume (SCM dynamicString, Real defaultValue);

private:
  // This performer queues a number of dynamic spans waiting for the following
  // pattern before computing their volume levels.
  //
  //  1. the first (de)crescendo, followed by ...
  //  2. zero or more spans that either change in the same direction as the
  //     first or do not change, followed by ...
  //  3. zero or more spans that either change in the opposite direction as the
  //     first or do not change
  //
  // The search may be cut short by an absolute dynamic or the end of the
  // context.
  enum State
  {
    STATE_INITIAL = 0, // waiting for a (de)crescendo
    STATE_DEPART, // enqueued the first span, gathering same-direction spans
    STATE_RETURN  // gathering opposite-direction spans
  };

  struct UnfinishedSpan
  {
    Audio_span_dynamic *dynamic_;
    Direction grow_dir_;

    UnfinishedSpan ()
      : dynamic_ (0),
        grow_dir_ (CENTER)
    {
    }
  };

  struct DynamicQueue
  {
    vector<UnfinishedSpan> spans_;
    // total duration of (de)crescendi (i.e. excluding fixed-volume spans)
    Real change_duration_;
    Real min_target_vol_;
    Real max_target_vol_;

    DynamicQueue ()
      : change_duration_ (0)
    {
    }

    void clear ()
    {
      spans_.clear ();
      change_duration_ = 0;
    }

    void push_back (const UnfinishedSpan &span, Real min_target_vol,
                    Real max_target_vol)
    {
      if (span.grow_dir_ != CENTER)
        change_duration_ += span.dynamic_->get_duration ();
      min_target_vol_ = min_target_vol;
      max_target_vol_ = max_target_vol;
      spans_.push_back (span);
    }

    void set_volume (Real start_vol, Real target_vol);
  };

private:
  vector<Audio_note *> notes_;
  Stream_event *script_event_;
  Drul_array<Stream_event *> span_events_;
  Direction next_grow_dir_;
  Direction depart_dir_;
  UnfinishedSpan open_span_;
  DynamicQueue depart_queue_;
  DynamicQueue return_queue_;
  State state_;
};

Dynamic_performer::Dynamic_performer (Context *c)
  : Performer (c),
    script_event_ (0),
    next_grow_dir_ (CENTER),
    depart_dir_ (CENTER),
    state_ (STATE_INITIAL)
{
  span_events_[LEFT] = span_events_[RIGHT] = 0;
}

void
Dynamic_performer::acknowledge_audio_element (Audio_element_info inf)
{
  // Keep track of the notes played in this translation time step so that they
  // can be pointed to the current dynamic in stop_translation_timestep.
  if (Audio_note *n = dynamic_cast<Audio_note *> (inf.elem_))
    {
      notes_.push_back (n);
    }
}

bool
Dynamic_performer::drive_state_machine (Direction next_grow_dir)
{
  switch (state_)
    {
    case STATE_INITIAL:
      if (next_grow_dir != CENTER)
        {
          state_ = STATE_DEPART;
          depart_dir_ = next_grow_dir;
        }
      break;

    case STATE_DEPART:
      if (next_grow_dir == -depart_dir_)
        state_ = STATE_RETURN;
      break;

    case STATE_RETURN:
      if (next_grow_dir == depart_dir_)
        {
          state_ = STATE_DEPART;
          return true;
        }
      break;
    }

  return false;
}

void
Dynamic_performer::close_and_enqueue_span ()
{
  if (!open_span_.dynamic_)
    programming_error ("no open dynamic span");
  else
    {
      DynamicQueue &dq
        = (state_ == STATE_RETURN) ? return_queue_ : depart_queue_;

      // Changing equalizer settings in the course of the performance does not
      // seem very likely.  This is a fig leaf: Equalize these limit volumes
      // now as the required context properties are current.  Note that only
      // the limits at the end of the last span in the queue are kept.

      // Resist diminishing to silence.  (Idea: Look up "ppppp"
      // with dynamicAbsoluteVolumeFunction, however that would yield 0.25.)
      const Real min_target = equalize_volume (0.1);
      const Real max_target
        = equalize_volume (Audio_span_dynamic::MAXIMUM_VOLUME);

      open_span_.dynamic_->set_end_moment (now_mom ());
      dq.push_back (open_span_, min_target, max_target);
    }

  open_span_ = UnfinishedSpan ();
}

// Set the starting and target volume for each span in the queue.  The gain
// (loss) of any (de)crescendo is proportional to its share of the total time
// spent changing.
void
Dynamic_performer::DynamicQueue::set_volume (Real start_vol, Real target_vol)
{
  const Real gain = target_vol - start_vol;
  Real dur = 0; // duration of (de)crescendi processed so far
  Real vol = start_vol;
  for (vector<UnfinishedSpan>::iterator it = spans_.begin ();
       it != spans_.end (); ++it)
    {
      const Real prev_vol = vol;
      if (it->grow_dir_ != CENTER)
        {
          // grant this (de)crescendo its portion of the gain
          dur += it->dynamic_->get_duration ();
          vol = start_vol + gain * (dur / change_duration_);
        }
      it->dynamic_->set_volume (prev_vol, vol);
    }
}

// Return a volume which is reasonably distant from the given start and end
// volumes in the given direction, for use as a peak volume in a passage with a
// crescendo followed by a decrescendo (or vice versa).  If the given volumes
// are equal, the returned volume is a also reasonable target volume for a
// single (de)crescendo.
//
// The given minimum and maximum volumes are the allowable dynamic range.
Real
Dynamic_performer::calc_departure_volume (Direction depart_dir, Real start_vol,
                                          Real end_vol, Real min_vol,
                                          Real max_vol)
{
  if (depart_dir == CENTER)
    return start_vol;

  // Try to find a volume that is a minimum distance from the starting and
  // ending volumes.  If the endpoint volumes differ, the nearer one is padded
  // less than the farther one.
  //
  // Example: mf < ... > p.  The legacy behavior was to use a 25% of the
  // dynamic range for a (de)crescendo to an unspecified target, and this tries
  // to preserve that, but is not possible to use a 25% change for both the
  // crescendo and the decrescendo and meet the constraints of this example.
  // The decrescendo is a greater change than the crescendo.  Believing that
  // 25% is already more than enough for either, pad using 25% for the greater
  // change and 7% for the lesser change.
  //
  // Idea: Use a context property or callback, e.g. the difference between two
  // dynamics in dynamicAbsoluteVolumeFunction.  0.25 is the default difference
  // between "p" and "ff". (Isn't that rather wide for this purpose?)  0.07 is
  // the default difference between "mp" and "mf".
  const Real far_padding = 0.25;
  const Real near_padding = 0.07;

  // If for some reason one of the endpoints is already below the supposed
  // minimum or maximum, just accept it.
  min_vol = std::min (std::min (min_vol, start_vol), end_vol);
  max_vol = std::max (std::max (max_vol, start_vol), end_vol);

  const Real vol_range = max_vol - min_vol;

  const Real near_vol = minmax (depart_dir, start_vol, end_vol)
                        + depart_dir * near_padding * vol_range;
  const Real far_vol = minmax (-depart_dir, start_vol, end_vol)
                       + depart_dir * far_padding * vol_range;
  const Real depart_vol = minmax (depart_dir, near_vol, far_vol);
  return std::max (std::min (depart_vol, max_vol), min_vol);
}

Real
Dynamic_performer::finish_queued_spans (Real next_vol)
{
  if (depart_queue_.spans_.empty ())
    {
      programming_error ("no dynamic span to finish");
      return next_vol;
    }

  const Real start_vol
    = depart_queue_.spans_.front ().dynamic_->get_start_volume ();

  if (return_queue_.spans_.empty ())
    {
      Real depart_vol = next_vol;

      // If the next dynamic is not specified or is inconsistent with the
      // direction of growth, choose a reasonable target.
      if ((next_vol < 0) || (depart_dir_ != Direction (next_vol - start_vol)))
        {
          depart_vol = calc_departure_volume (depart_dir_, start_vol, start_vol,
                                              depart_queue_.min_target_vol_,
                                              depart_queue_.max_target_vol_);
        }

      depart_queue_.set_volume (start_vol, depart_vol);
      depart_queue_.clear ();
      return (next_vol >= 0) ? next_vol : depart_vol;
    }
  else
    {
      // If the next dynamic is not specified, return to the starting volume.
      const Real return_vol = (next_vol >= 0) ? next_vol : start_vol;
      Real depart_vol = calc_departure_volume (
        depart_dir_, start_vol, return_vol, depart_queue_.min_target_vol_,
        depart_queue_.max_target_vol_);
      depart_queue_.set_volume (start_vol, depart_vol);
      depart_queue_.clear ();
      return_queue_.set_volume (depart_vol, return_vol);
      return_queue_.clear ();
      return return_vol;
    }
}

Real
Dynamic_performer::equalize_volume (Real volume)
{
  /*
    properties override default equaliser setting
  */
  SCM min = get_property (this, "midiMinimumVolume");
  SCM max = get_property (this, "midiMaximumVolume");
  if (scm_is_number (min) || scm_is_number (max))
    {
      Interval iv (Audio_span_dynamic::MINIMUM_VOLUME,
                   Audio_span_dynamic::MAXIMUM_VOLUME);
      if (scm_is_number (min))
        iv[DOWN] = from_scm<double> (min);
      if (scm_is_number (max))
        iv[UP] = from_scm<double> (max);
      volume = iv[DOWN] + iv.length () * volume;
    }
  else
    {
      /*
        urg, code duplication:: staff_performer
      */
      SCM s = get_property (this, "midiInstrument");

      if (!scm_is_string (s))
        s = get_property (this, "instrumentName");

      if (!scm_is_string (s))
        s = scm_from_latin1_string ("piano");

      SCM eq = get_property (this, "instrumentEqualizer");
      if (ly_is_procedure (eq))
        s = ly_call (eq, s);

      if (is_number_pair (s))
        {
          Interval iv = from_scm<Interval> (s);
          volume = iv[DOWN] + iv.length () * volume;
        }
    }
  return std::max (std::min (volume, Audio_span_dynamic::MAXIMUM_VOLUME),
                   Audio_span_dynamic::MINIMUM_VOLUME);
}

void
Dynamic_performer::finalize ()
{
  if (open_span_.dynamic_)
    close_and_enqueue_span ();
  finish_queued_spans ();
}

Real
Dynamic_performer::look_up_absolute_volume (SCM dynamicString,
                                            Real defaultValue)
{
  SCM proc = get_property (this, "dynamicAbsoluteVolumeFunction");

  SCM svolume = SCM_EOL;
  if (ly_is_procedure (proc))
    svolume = ly_call (proc, dynamicString);

  return from_scm<double> (svolume, defaultValue);
}

void
Dynamic_performer::process_music ()
{
  Real volume = -1;

  if (script_event_) // explicit dynamic
    {
      volume = look_up_absolute_volume (get_property (script_event_, "text"),
                                        Audio_span_dynamic::DEFAULT_VOLUME);
      volume = equalize_volume (volume);
    }
  else if (!open_span_.dynamic_) // first time only
    {
      // Idea: look_up_absolute_volume (ly_symbol2scm ("mf")).
      // It is likely to change regtests.
      volume = equalize_volume (Audio_span_dynamic::DEFAULT_VOLUME);
    }

  // end the current span at relevant points
  if (open_span_.dynamic_
      && (span_events_[START] || span_events_[STOP] || script_event_))
    {
      close_and_enqueue_span ();
      if (script_event_)
        {
          state_ = STATE_INITIAL;
          volume = finish_queued_spans (volume);
        }
    }

  // start a new span so that some dynamic is always in effect
  if (!open_span_.dynamic_)
    {
      if (drive_state_machine (next_grow_dir_))
        volume = finish_queued_spans (volume);

      // if not known by now, use a default volume for robustness
      if (volume < 0)
        volume = equalize_volume (Audio_span_dynamic::DEFAULT_VOLUME);

      Stream_event *cause = span_events_[START] ? span_events_[START]
                            : script_event_     ? script_event_
                                                : span_events_[STOP];

      open_span_.dynamic_ = new Audio_span_dynamic (now_mom (), volume);
      open_span_.grow_dir_ = next_grow_dir_;
      announce_element (Audio_element_info (open_span_.dynamic_, cause));
    }
}

void
Dynamic_performer::stop_translation_timestep ()
{
  // link notes to the current dynamic
  if (!open_span_.dynamic_)
    programming_error ("no current dynamic");
  else
    {
      for (vector<Audio_note *>::const_iterator ni = notes_.begin ();
           ni != notes_.end (); ++ni)
        {
          (*ni)->dynamic_ = open_span_.dynamic_;
        }
    }
  notes_.clear ();

  script_event_ = 0;
  span_events_[LEFT] = span_events_[RIGHT] = 0;
  next_grow_dir_ = CENTER;
}

void
Dynamic_performer::listen_decrescendo (Stream_event *r)
{
  Direction d = from_scm<Direction> (get_property (r, "span-direction"));
  if (assign_event_once (span_events_[d], r) && (d == START))
    next_grow_dir_ = SMALLER;
}

void
Dynamic_performer::listen_crescendo (Stream_event *r)
{
  Direction d = from_scm<Direction> (get_property (r, "span-direction"));
  if (assign_event_once (span_events_[d], r) && (d == START))
    next_grow_dir_ = BIGGER;
}

void
Dynamic_performer::listen_absolute_dynamic (Stream_event *r)
{
  assign_event_once (script_event_, r);
}

void
Dynamic_performer::boot ()
{
  ADD_LISTENER (decrescendo);
  ADD_LISTENER (crescendo);
  ADD_LISTENER (absolute_dynamic);
}

ADD_TRANSLATOR (Dynamic_performer,
                /* doc */
                R"(

                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
dynamicAbsoluteVolumeFunction
instrumentEqualizer
midiMaximumVolume
midiMinimumVolume
midiInstrument
                )",

                /* write */
                R"(

                )");
