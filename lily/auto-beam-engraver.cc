/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "beaming-pattern.hh"
#include "beam.hh"
#include "context.hh"
#include "context-handle.hh"
#include "duration.hh"
#include "engraver.hh"
#include "grob-properties.hh"
#include "item.hh"
#include "rest.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "stem.hh"
#include "warn.hh"

#include "translator.icc"

using std::vector;

class Auto_beam_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Auto_beam_engraver);

protected:
  void stop_translation_timestep ();
  void process_acknowledged ();

  virtual void process_music ();
  void finalize () override;
  void derived_mark () const override;

  void acknowledge_rest (Grob_info);
  void acknowledge_beam (Grob_info);
  void acknowledge_breathing_sign (Grob_info);
  void acknowledge_stem (Grob_info);
  void listen_beam_forbid (Stream_event *);

private:
  virtual bool test_moment (Direction, Moment, Moment);
  bool busy () const { return beam_start_moment_ < Moment::infinity (); }
  void consider_begin (Moment, Moment);
  void consider_end (Moment, Moment);
  Spanner *create_beam ();
  void begin_beam ();
  void end_beam ();
  void handle_current_stem (Item *stem);
  void junk_beam ();
  virtual bool is_same_grace_state (Moment, Moment);
  void recheck_beam ();
  void typeset_beam ();

  Stream_event *forbid_ = nullptr;
  bool force_end_ = false;
  bool considered_bar_ = false;
  /*
    shortest_mom_ is the shortest note in the beam.
  */
  Moment shortest_mom_ {Rational (1, 4)};
  Spanner *finished_beam_ = nullptr;
  // This engraver is designed to operate in Voice context, so we expect only
  // one stem per timestep.
  Item *current_stem_ = nullptr;
  vector<Item *> stems_;

  int process_acknowledged_count_ = 0;
  Moment last_add_mom_;
  /*
    Projected ending of the  beam we're working on.
  */
  Moment extend_mom_ {-1};
  Moment beam_start_moment_ = Moment::infinity ();
  Moment beam_start_location_;
  /*
    Handle on the starting staff keeps it alive until beam is comlete
  */
  Context_handle beam_start_context_;

  // We act as if beam were created, and start a grouping anyway.
  Beaming_pattern *grouping_ = nullptr;
  SCM beam_settings_ = SCM_EOL;

  Beaming_pattern *finished_grouping_ = nullptr;

  Beaming_options beaming_options_;
  Beaming_options finished_beaming_options_;
};

void
Auto_beam_engraver::derived_mark () const
{
  scm_gc_mark (beam_settings_);
}

void
Auto_beam_engraver::process_music ()
{
  Moment now = now_mom ();
  /*
    don't beam over skips
  */
  if (busy ())
    {
      if (extend_mom_ < now)
        end_beam ();
    }
}

Auto_beam_engraver::Auto_beam_engraver (Context *c)
  : Engraver (c)
{
}

void
Auto_beam_engraver::listen_beam_forbid (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (forbid_, ev);
  force_end_ = true;
}

bool
Auto_beam_engraver::test_moment (Direction dir, Moment test_mom, Moment dur)
{
  return scm_is_true (scm_call_4 (get_property (this, "autoBeamCheck"),
                                  context ()->self_scm (),
                                  to_scm (dir),
                                  test_mom.smobbed_copy (),
                                  dur.smobbed_copy ()));
}

void
Auto_beam_engraver::consider_begin (Moment test_mom, Moment dur)
{
  if (!busy () && !forbid_
      && from_scm<bool> (get_property (this, "autoBeaming"))
      && test_moment (START, test_mom, dur))
    {
      begin_beam ();
    }
}

void
Auto_beam_engraver::consider_end (Moment test_mom, Moment dur)
{
  if (busy ())
    {
      /* Allow already started autobeam to end:
         don't check for autoBeaming */
      bool b = test_moment (STOP, test_mom, dur);
      if (b)
        end_beam ();
    }
}

Spanner *
Auto_beam_engraver::create_beam ()
{
  if (from_scm<bool> (get_property (this, "skipTypesetting")))
    return 0;

  for (const auto &stem : stems_)
    if (Stem::get_beam (stem))
      return 0;

  /*
    Can't use make_spanner () because we have to use
    beam_settings_.
  */
  Spanner *beam = new Spanner (beam_settings_);

  for (const auto &stem : stems_)
    Beam::add_stem (beam, stem);

  Grob_info i = make_grob_info (beam, stems_[0]->self_scm ());
  announce_grob (i, beam_start_context_.get_context ());

  return beam;
}

void
Auto_beam_engraver::begin_beam ()
{
  if (busy () || grouping_)
    {
      programming_error ("already have autobeam");
      return;
    }

  stems_.clear ();
  grouping_ = new Beaming_pattern ();
  beaming_options_.from_context (context ());
  beam_settings_ = Grob_property_info (context (), ly_symbol2scm ("Beam")).updated ();

  beam_start_context_.set_context (context ()->get_parent ());
  beam_start_location_
    = robust_scm2moment (get_property (this, "measurePosition"), Moment (0));
  beam_start_moment_ = now_mom ();
}

void
Auto_beam_engraver::junk_beam ()
{
  if (!busy ())
    return;

  beam_start_moment_ = Moment::infinity ();
  stems_.clear ();
  delete grouping_;
  grouping_ = 0;
  beam_settings_ = SCM_EOL;

  shortest_mom_ = Moment (Rational (1, 4));
}

bool
Auto_beam_engraver::is_same_grace_state (Moment start, Moment now)
{
  return bool (start.grace_part_) == bool (now.grace_part_);
}

void
Auto_beam_engraver::end_beam ()
{
  if (stems_.size () < 2)
    junk_beam ();
  else
    {
      finished_beam_ = create_beam ();

      if (finished_beam_)
        {
          Grob_info i = make_grob_info (finished_beam_, SCM_EOL);

          announce_end_grob (i, beam_start_context_.get_context ());
          finished_grouping_ = grouping_;
          finished_beaming_options_ = beaming_options_;
        }
      beam_start_moment_ = Moment::infinity ();
      stems_.clear ();
      grouping_ = 0;
      beam_settings_ = SCM_EOL;
    }

  beam_start_context_.set_context (NULL);
  shortest_mom_ = Moment (Rational (1, 4));
}

void
Auto_beam_engraver::typeset_beam ()
{
  if (finished_beam_)
    {
      if (!finished_beam_->get_bound (RIGHT))
        finished_beam_->set_bound (RIGHT, finished_beam_->get_bound (LEFT));

      finished_grouping_->beamify (finished_beaming_options_);
      Beam::set_beaming (finished_beam_, finished_grouping_);
      finished_beam_ = 0;

      delete finished_grouping_;
      finished_grouping_ = 0;
    }
}

void
Auto_beam_engraver::stop_translation_timestep ()
{
  typeset_beam ();
  process_acknowledged_count_ = 0;
  forbid_ = 0;
  considered_bar_ = false;
}

void
Auto_beam_engraver::finalize ()
{
  /* finished beams may be typeset */
  typeset_beam ();
  /* but unfinished may need another announce/acknowledge pass */
  if (busy ())
    junk_beam ();
}

void
Auto_beam_engraver::acknowledge_beam (Grob_info /* info */)
{
  force_end_ = true;
}

void
Auto_beam_engraver::acknowledge_breathing_sign (Grob_info /* info */)
{
  force_end_ = true;
}

void
Auto_beam_engraver::acknowledge_rest (Grob_info /* info */)
{
  force_end_ = true;
}

void
Auto_beam_engraver::acknowledge_stem (Grob_info info)
{
  current_stem_ = dynamic_cast<Item *> (info.grob ());
}

void
Auto_beam_engraver::handle_current_stem (Item *stem)
{
  auto *const ev = stem->ultimate_event_cause ();
  if (!ev->in_event_class ("rhythmic-event"))
    {
      programming_error ("stem must have rhythmic structure");
      return;
    }

  /*
    Don't (start) auto-beam over empty stems; skips or rests
  */
  if (!Stem::head_count (stem))
    {
      if (busy ())
        end_beam ();
      return;
    }

  if (Stem::get_beam (stem))
    {
      if (busy ())
        junk_beam ();
      return;
    }

  auto *const stem_duration = unsmob<Duration> (get_property (ev, "duration"));
  const auto durlog = stem_duration->duration_log ();

  if (durlog <= 2)
    {
      if (busy ())
        end_beam ();
      return;
    }

  /*
    ignore interspersed grace notes.
  */
  Moment now = now_mom ();
  if (!is_same_grace_state (beam_start_location_, now))
    return;

  Moment dur (stem_duration->get_length ());
  Moment measure_now = measure_position (context ());
  bool recheck_needed = false;

  if (dur < shortest_mom_)
    {
      /* new shortest moment, so store it and set recheck_needed */
      shortest_mom_ = dur;
      recheck_needed = true;
    }

  /* end should be based on shortest_mom_, begin should be
     based on current duration  */
  consider_end (measure_now, shortest_mom_);
  consider_begin (measure_now, dur);

  if (!busy ())
    return;

  grouping_->add_stem (now - beam_start_moment_ + beam_start_location_,
                       durlog - 2,
                       Stem::is_invisible (stem),
                       stem_duration->factor (),
                       (from_scm<bool> (get_property (stem, "tuplet-start"))));
  stems_.push_back (stem);
  last_add_mom_ = now;
  extend_mom_ = std::max (extend_mom_, now) + get_event_length (ev, now);
  if (recheck_needed)
    recheck_beam ();
}

void
Auto_beam_engraver::recheck_beam ()
{
  /*
    Recheck the beam after the shortest duration has changed
    If shorter duration has created a new break, typeset the
    first part of the beam and reset the current beam to just
    the last part of the beam
  */
  Beaming_pattern *new_grouping_ = 0;
  vector<Item *> new_stems;
  Moment temporary_shortest_mom;
  SCM temporary_beam_settings;

  bool found_end;

  for (vsize i = 0; (i + 1) < stems_.size (); /*in body*/)
    {
      found_end = test_moment (STOP,
                               grouping_->end_moment (i),
                               shortest_mom_);
      if (!found_end)
        i++;
      else
        {
          /*
            Save the current beam settings and shortest_mom_
            Necessary because end_beam destroys them
          */
          temporary_shortest_mom = shortest_mom_;
          temporary_beam_settings = beam_settings_;

          /* Eliminate (and save) the items no longer part of the first beam */

          new_grouping_ = grouping_->split_pattern (i);
          new_stems.insert (new_stems.end (),
                            std::next (stems_.begin (), i + 1),
                            stems_.end ());
          stems_.resize (i + 1);

          end_beam ();
          typeset_beam ();

          /* now recreate the unbeamed data structures */
          stems_ = std::move (new_stems);
          new_stems.clear ();
          grouping_ = new_grouping_;
          shortest_mom_ = temporary_shortest_mom;
          beam_settings_ = temporary_beam_settings;
          beam_start_moment_ = now_mom ();

          i = 0;
        }

    }

}

void
Auto_beam_engraver::process_acknowledged ()
{
  if (force_end_)
    {
      force_end_ = false;

      if (busy ())
        end_beam ();
    }

  // This engraver can't observe bar lines with acknowledge_bar_line () because
  // the Bar_engraver operates in Staff context.  Instead, we check the
  // whichBar property, which also controls the Bar_engraver.
  // process_acknowledged () can be called more than once, but whichBar won't
  // change.
  if (!considered_bar_)
    {
      considered_bar_ = true;

      if (busy () && scm_is_string (get_property (this, "whichBar")))
        {
          consider_end (measure_position (context ()), shortest_mom_);
          junk_beam ();
        }
    }

  if (current_stem_)
    {
      handle_current_stem (current_stem_);
      current_stem_ = nullptr;
    }

  Moment now = now_mom ();
  if (extend_mom_ > now)
    return;

  if (busy ())
    {
      if (!process_acknowledged_count_)
        {
          consider_end (measure_position (context ()), shortest_mom_);
        }
      else if (process_acknowledged_count_ > 1)
        {
          if ((extend_mom_ < now)
              || ((extend_mom_ == now) && (last_add_mom_ != now)))
            end_beam ();
          else if (stems_.empty ())
            junk_beam ();
        }
    }

  process_acknowledged_count_++;
}

void
Auto_beam_engraver::boot ()
{
  ADD_LISTENER (Auto_beam_engraver, beam_forbid);
  ADD_ACKNOWLEDGER (Auto_beam_engraver, stem);
  ADD_ACKNOWLEDGER (Auto_beam_engraver, beam);
  ADD_ACKNOWLEDGER (Auto_beam_engraver, breathing_sign);
  ADD_ACKNOWLEDGER (Auto_beam_engraver, rest);
}

ADD_TRANSLATOR (Auto_beam_engraver,
                /* doc */
                "Generate beams based on measure characteristics and observed"
                " Stems.  Uses @code{baseMoment}, @code{beatStructure},"
                " @code{beamExceptions}, @code{measureLength}, and"
                " @code{measurePosition} to decide when to start and stop a"
                " beam.  Overriding beaming is done through"
                " @ref{Stem_engraver} properties @code{stemLeftBeamCount} and"
                " @code{stemRightBeamCount}.",

                /* create */
                "Beam ",

                /* read */
                "autoBeaming "
                "baseMoment "
                "beamExceptions "
                "beamHalfMeasure "
                "beatStructure "
                "subdivideBeams ",

                /* write */
                ""
               );

class Grace_auto_beam_engraver : public Auto_beam_engraver
{
  TRANSLATOR_DECLARATIONS (Grace_auto_beam_engraver);

private:
  // Full starting time of last grace group.  grace_part_ is zero ->
  // test_moment is false, last_grace_position_ not considered.
  Moment last_grace_start_ {-Rational::infinity ()};
  Moment last_grace_position_; // Measure position of same
  void process_music () override;
  bool is_same_grace_state (Moment, Moment) override;
  bool test_moment (Direction, Moment, Moment) override;
};

Grace_auto_beam_engraver::Grace_auto_beam_engraver (Context *c)
  : Auto_beam_engraver (c)
{
}

bool
Grace_auto_beam_engraver::is_same_grace_state (Moment, Moment)
{
  // This is for ignoring interspersed grace notes in main note
  // beaming.  We never want to ignore something inside of grace note
  // beaming, so return true.
  return true;
}

void
Grace_auto_beam_engraver::process_music ()
{
  Moment now = now_mom ();
  // Update last_grace_start_ and last_grace_position_ only when the
  // main time advances.
  if (now.main_part_ > last_grace_start_.main_part_)
    {
      last_grace_start_ = now;
      last_grace_position_ = measure_position (context ());
    }

  Auto_beam_engraver::process_music ();
}

bool
Grace_auto_beam_engraver::test_moment (Direction dir, Moment test_mom, Moment)
{
  // If no grace group started this main moment, we have no business
  // beaming.  Same if we have left the original main time step.
  if (!last_grace_start_.grace_part_
      || last_grace_position_.main_part_ != test_mom.main_part_)
    return false;
  // Autobeam start only when at the start of the grace group.
  if (dir == START)
    return last_grace_position_ == test_mom;
  // Autobeam end only when the grace part is finished.
  return !test_mom.grace_part_;
}

void
Grace_auto_beam_engraver::boot ()
{
  ADD_LISTENER (Grace_auto_beam_engraver, beam_forbid);
  ADD_ACKNOWLEDGER (Grace_auto_beam_engraver, stem);
  ADD_ACKNOWLEDGER (Grace_auto_beam_engraver, beam);
  ADD_ACKNOWLEDGER (Grace_auto_beam_engraver, breathing_sign);
  ADD_ACKNOWLEDGER (Grace_auto_beam_engraver, rest);
}

ADD_TRANSLATOR (Grace_auto_beam_engraver,
                /* doc */
                "Generates one autobeam group across an entire grace phrase. "
                " As usual, any manual beaming or @code{\\noBeam} will block"
                " autobeaming, just like setting the context property"
                " @samp{autoBeaming} to @code{##f}.",

                /* create */
                "Beam ",

                /* read */
                "autoBeaming ",

                /* write */
                ""
               );
