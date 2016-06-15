/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2015 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "std-vector.hh"
#include "stream-event.hh"
#include "international.hh"

#include "translator.icc"

class Dynamic_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Dynamic_performer);
protected:
  virtual void finalize ();
  void stop_translation_timestep ();
  void process_music ();
  Real equalize_volume (Real);

  void listen_decrescendo (Stream_event *);
  void listen_crescendo (Stream_event *);
  void listen_absolute_dynamic (Stream_event *);

private:
  // next_vol < 0 means select a target dynamic based on growth direction.
  // return actual next volume (computed if not provided)
  Real end_span (Real next_vol = -1.0);

private:
  Stream_event *script_event_;
  Drul_array<Stream_event *> span_events_;
  Direction next_grow_dir_;
  Audio_span_dynamic *span_dynamic_;
  Direction grow_dir_; // of span_dynamic_
};

Dynamic_performer::Dynamic_performer ()
{
  script_event_ = 0;
  span_events_[LEFT]
    = span_events_[RIGHT] = 0;
  next_grow_dir_ = CENTER;
  span_dynamic_ = 0;
  grow_dir_ = CENTER;
}

Real Dynamic_performer::end_span (Real next_vol)
{
  if (!span_dynamic_)
    {
      programming_error("no dynamic span to end");
      return next_vol;
    }

  Real start_vol = span_dynamic_->get_start_volume ();
  Real target_vol = start_vol;

  if (grow_dir_ != CENTER) {
    // If the target dynamic is not specified, grow to a reasonable target
    // in the desired direction.  Do the same for cases like mf < p.
    //
    // TODO To improve on this, keep a queue of Audio_span_dynamics and compute
    // multiple intermediate targets based on the next explicit dynamic.
    // Consider cases like mf < ... < ff with only mf and ff specified.
    // Consider growing in proportion to the duration of each (de)crescendo in
    // the sequence, which may be separated by spans with no change in volume.
    if ((next_vol < 0) || (sign(next_vol - start_vol) != grow_dir_))
      {
        Real min_vol = equalize_volume (0.1);
        Real max_vol = equalize_volume (Audio_span_dynamic::MAXIMUM_VOLUME);
        target_vol = max (min (start_vol + grow_dir_ * (max_vol - min_vol) * 0.25, max_vol), min_vol);
      }
    else
      {
        target_vol = next_vol;
      }
  }

  span_dynamic_->set_end_moment (now_mom ());
  span_dynamic_->set_volume (start_vol, target_vol);
  span_dynamic_ = 0;

  return (next_vol >= 0) ? next_vol : target_vol;
}

Real
Dynamic_performer::equalize_volume (Real volume)
{
  /*
    properties override default equaliser setting
  */
  SCM min = get_property ("midiMinimumVolume");
  SCM max = get_property ("midiMaximumVolume");
  if (scm_is_number (min) || scm_is_number (max))
    {
      Interval iv (Audio_span_dynamic::MINIMUM_VOLUME,
                   Audio_span_dynamic::MAXIMUM_VOLUME);
      if (scm_is_number (min))
        iv[MIN] = scm_to_double (min);
      if (scm_is_number (max))
        iv[MAX] = scm_to_double (max);
      volume = iv[MIN] + iv.length () * volume;
    }
  else
    {
      /*
        urg, code duplication:: staff_performer
      */
      SCM s = get_property ("midiInstrument");

      if (!scm_is_string (s))
        s = get_property ("instrumentName");

      if (!scm_is_string (s))
        s = scm_from_ascii_string ("piano");

      SCM eq = get_property ("instrumentEqualizer");
      if (ly_is_procedure (eq))
        s = scm_call_1 (eq, s);

      if (is_number_pair (s))
        {
          Interval iv = ly_scm2interval (s);
          volume = iv[MIN] + iv.length () * volume;
        }
    }
  return std::max (std::min (volume, Audio_span_dynamic::MAXIMUM_VOLUME),
                   Audio_span_dynamic::MINIMUM_VOLUME);
}

void
Dynamic_performer::finalize ()
{
  if (span_dynamic_)
    {
      end_span ();
    }
}

void
Dynamic_performer::process_music ()
{
  Real volume = -1;

  if (script_event_)
    {
      // Explicit dynamic script event: determine the volume.
      SCM proc = get_property ("dynamicAbsoluteVolumeFunction");

      SCM svolume = SCM_EOL;
      if (ly_is_procedure (proc))
        {
          // urg
          svolume = scm_call_1 (proc, script_event_->get_property ("text"));
        }

      volume = equalize_volume (robust_scm2double (svolume, Audio_span_dynamic::DEFAULT_VOLUME));
    }
  else if (!span_dynamic_) // first time through
    {
      volume = equalize_volume (Audio_span_dynamic::DEFAULT_VOLUME);
    }

  // end the current span at relevant points
  if (span_dynamic_
      && (span_events_[START] || span_events_[STOP] || script_event_))
    {
      volume = end_span (volume);
    }

  // start a new span so that some dynamic is always in effect
  if (!span_dynamic_)
    {
      Stream_event *cause =
        span_events_[START] ? span_events_[START] :
        script_event_ ? script_event_ :
        span_events_[STOP];

      span_dynamic_ = new Audio_span_dynamic (now_mom (), volume);
      grow_dir_ = next_grow_dir_;
      announce_element (Audio_element_info (span_dynamic_, cause));
    }
}

void
Dynamic_performer::stop_translation_timestep ()
{
  script_event_ = 0;
  span_events_[LEFT]
    = span_events_[RIGHT] = 0;
  next_grow_dir_ = CENTER;
}

void
Dynamic_performer::listen_decrescendo (Stream_event *r)
{
  Direction d = to_dir (r->get_property ("span-direction"));
  if (ASSIGN_EVENT_ONCE (span_events_[d], r) && (d == START))
    next_grow_dir_ = SMALLER;
}

void
Dynamic_performer::listen_crescendo (Stream_event *r)
{
  Direction d = to_dir (r->get_property ("span-direction"));
  if (ASSIGN_EVENT_ONCE (span_events_[d], r) && (d == START))
    next_grow_dir_ = BIGGER;
}

void
Dynamic_performer::listen_absolute_dynamic (Stream_event *r)
{
  ASSIGN_EVENT_ONCE (script_event_, r);
}

void
Dynamic_performer::boot ()
{
  ADD_LISTENER (Dynamic_performer, decrescendo);
  ADD_LISTENER (Dynamic_performer, crescendo);
  ADD_LISTENER (Dynamic_performer, absolute_dynamic);
}

ADD_TRANSLATOR (Dynamic_performer,
                /* doc */
                "",

                /* create */
                "",

                /* read */
                "dynamicAbsoluteVolumeFunction "
                "instrumentEqualizer "
                "midiMaximumVolume "
                "midiMinimumVolume "
                "midiInstrument ",

                /* write */
                ""
               );
