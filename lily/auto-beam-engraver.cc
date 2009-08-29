/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2009 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "bar-line.hh"
#include "beaming-pattern.hh"
#include "beam.hh"
#include "context.hh"
#include "duration.hh"
#include "engraver.hh"
#include "item.hh"
#include "rest.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "stem.hh"
#include "warn.hh"

#include "translator.icc"

class Auto_beam_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Auto_beam_engraver);

protected:
  void stop_translation_timestep ();
  void process_music ();
  virtual void finalize ();
  virtual void derived_mark () const;

  DECLARE_ACKNOWLEDGER (rest);
  DECLARE_ACKNOWLEDGER (beam);
  DECLARE_ACKNOWLEDGER (bar_line);
  DECLARE_ACKNOWLEDGER (stem);
  DECLARE_TRANSLATOR_LISTENER (beam_forbid);

  void process_acknowledged ();

private:
  bool test_moment (Direction, Moment, Moment);
  void consider_begin (Moment, Moment);
  void consider_end (Moment, Moment);
  Spanner *create_beam ();
  void begin_beam ();
  void end_beam ();
  void junk_beam ();
  bool is_same_grace_state (Grob *e);
  void recheck_beam ();
  void typeset_beam ();
  vector<Item*> *remove_end_stems (vsize);

  Stream_event *forbid_;
  /*
    shortest_mom_ is the shortest note in the beam.
  */
  Moment shortest_mom_;
  Spanner *finished_beam_;
  vector<Item*> *stems_;

  int process_acknowledged_count_;
  Moment last_add_mom_;
  /*
    Projected ending of the  beam we're working on.
  */
  Moment extend_mom_;
  Moment beam_start_moment_;
  Moment beam_start_location_;

  // We act as if beam were created, and start a grouping anyway.
  Beaming_pattern *grouping_;
  SCM beam_settings_;

  Beaming_pattern *finished_grouping_;


  Beaming_options beaming_options_;
  Beaming_options finished_beaming_options_;


  void check_bar_property ();
};

void
Auto_beam_engraver::derived_mark () const
{
  scm_gc_mark (beam_settings_);
}

void
Auto_beam_engraver::check_bar_property ()
{
  /* Duplicated from process_music (), since
     Repeat_acknowledge_engraver::process_music () may also set whichBar.  */

  Moment now = now_mom ();
  if (scm_is_string (get_property ("whichBar"))
      && beam_start_moment_ < now)
    {
      consider_end (measure_position (context ()), shortest_mom_);
      junk_beam ();
    }
}

void
Auto_beam_engraver::process_music ()
{
  Moment now = now_mom ();
  /*
    don't beam over skips
  */
  if (stems_)
    {
      if (extend_mom_ < now)
	end_beam ();
    }

  if (scm_is_string (get_property ("whichBar")))
    {
      consider_end (measure_position (context ()), shortest_mom_);
      junk_beam ();
    }

  if (forbid_)
    {
      consider_end (measure_position (context ()), shortest_mom_);
      junk_beam ();
    }
}

Auto_beam_engraver::Auto_beam_engraver ()
{
  forbid_ = 0;
  process_acknowledged_count_ = 0;
  stems_ = 0;
  shortest_mom_ = Moment (Rational (1, 8));
  finished_beam_ = 0;
  finished_grouping_ = 0;
  grouping_ = 0;
  beam_settings_ = SCM_EOL;
}

IMPLEMENT_TRANSLATOR_LISTENER (Auto_beam_engraver, beam_forbid);
void
Auto_beam_engraver::listen_beam_forbid (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (forbid_, ev);
}

bool
Auto_beam_engraver::test_moment (Direction dir, Moment test_mom, Moment dur)
{
  return scm_call_4 (get_property ("autoBeamCheck"),
		     context ()->self_scm (),
		     scm_from_int (dir),
                     test_mom.smobbed_copy(),
		     dur.smobbed_copy ())
    != SCM_BOOL_F;
}

void
Auto_beam_engraver::consider_begin (Moment test_mom, Moment dur)
{
  bool on = to_boolean (get_property ("autoBeaming"));
  if (!stems_ && on
      && !forbid_)
    {
      bool b = test_moment (START, test_mom, dur);
      if (b)
	begin_beam ();
    }
}

void
Auto_beam_engraver::consider_end (Moment test_mom, Moment dur)
{
  if (stems_)
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
  if (to_boolean (get_property ("skipTypesetting")))
    return 0;

  for (vsize i = 0; i < stems_->size (); i++)
    if (Stem::get_beam ((*stems_)[i]))
      return 0;

  /*
    Can't use make_spanner_from_properties () because we have to use
    beam_settings_.
  */
  Spanner *beam = new Spanner (beam_settings_);

  for (vsize i = 0; i < stems_->size (); i++)
    Beam::add_stem (beam, (*stems_)[i]);

  announce_grob (beam, (*stems_)[0]->self_scm ());

  return beam;
}

void
Auto_beam_engraver::begin_beam ()
{
  if (stems_ || grouping_)
    {
      programming_error ("already have autobeam");
      return;
    }

  stems_ = new vector<Item*>;
  grouping_ = new Beaming_pattern ();
  beaming_options_.from_context (context ());
  beam_settings_ = updated_grob_properties (context (), ly_symbol2scm ("Beam"));

  beam_start_moment_ = now_mom ();
  beam_start_location_
    = robust_scm2moment (get_property ("measurePosition"), Moment (0));
}

void
Auto_beam_engraver::junk_beam ()
{
  if (!stems_)
    return;

  delete stems_;
  stems_ = 0;
  delete grouping_;
  grouping_ = 0;
  beam_settings_ = SCM_EOL;

  shortest_mom_ = Moment (Rational (1, 8));
}

void
Auto_beam_engraver::end_beam ()
{
  if (stems_->size () < 2)
    junk_beam ();
  else
    {
      finished_beam_ = create_beam ();

      if (finished_beam_)
	{
	  announce_end_grob (finished_beam_, SCM_EOL);
	  finished_grouping_ = grouping_;
	  finished_beaming_options_ = beaming_options_;
	}
      delete stems_;
      stems_ = 0;
      grouping_ = 0;
      beam_settings_ = SCM_EOL;
    }

  shortest_mom_ = Moment (Rational (1, 8));
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
}

void
Auto_beam_engraver::finalize ()
{
  /* finished beams may be typeset */
  typeset_beam ();
  /* but unfinished may need another announce/acknowledge pass */
  if (stems_)
    junk_beam ();
}


void
Auto_beam_engraver::acknowledge_beam (Grob_info /* info */)
{
  check_bar_property ();
  if (stems_)
    end_beam ();
}

void
Auto_beam_engraver::acknowledge_bar_line (Grob_info /* info */)
{
  check_bar_property ();
  if (stems_)
    end_beam ();
}

void
Auto_beam_engraver::acknowledge_rest (Grob_info /* info */)
{
  check_bar_property ();
  if (stems_)
    end_beam ();
}

void
Auto_beam_engraver::acknowledge_stem (Grob_info info)
{
  check_bar_property ();
  Item *stem = dynamic_cast<Item *> (info.grob ());
  Stream_event *ev = info.ultimate_event_cause ();
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
      if (stems_)
	end_beam ();
      return;
    }

  if (Stem::get_beam (stem))
    {
      if (stems_)
	junk_beam ();
      return;
    }

  int durlog = unsmob_duration (ev->get_property ("duration"))->duration_log ();

  if (durlog <= 2)
    {
      if (stems_)
	end_beam ();
      return;
    }

  /*
    ignore grace notes.
  */
  Moment now = now_mom ();
  if (bool (beam_start_location_.grace_part_) != bool (now.grace_part_))
    return;

  Moment ev_dur = unsmob_duration (ev->get_property ("duration"))->get_length ();
  Moment dur = Rational (1, ev_dur.den ());
  Moment measure_now = measure_position (context ());
  bool recheck_needed = 0;

  if (dur < shortest_mom_)
    {
    /* new shortest moment, so store it and set recheck_needed */
    shortest_mom_ = dur;
    recheck_needed = 1;
    }

  /* end should be based on shortest_mom_, begin should be
     based on current duration  */
  consider_end (measure_now, shortest_mom_);
  consider_begin (measure_now, dur);

  if (!stems_)
    return;

  grouping_->add_stem (now - beam_start_moment_ + beam_start_location_,
		       durlog - 2,
		       Stem::is_invisible (stem));
  stems_->push_back (stem);
  last_add_mom_ = now;
  extend_mom_ = max (extend_mom_, now) + get_event_length (ev, now);
  if (recheck_needed) recheck_beam ();
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
  vector<Item*> *new_stems_ = 0;
  Moment temporary_shortest_mom;
  SCM temporary_beam_settings;

  bool found_end;


  for (vsize i = 0; i < stems_->size () - 1; )
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
          new_stems_ = remove_end_stems (i);

          end_beam ();
          typeset_beam ();

          /* now recreate the unbeamed data structures */
          stems_ = new_stems_;
          grouping_ = new_grouping_;
          shortest_mom_ = temporary_shortest_mom;
          beam_settings_ = temporary_beam_settings;

          i = 0;
        }

    }

}

/*
  Remove all stems with an index greater than split_index
  from stems_, and return a vector containing all of the
  removed stems
*/
vector <Item*> *
Auto_beam_engraver::remove_end_stems (vsize split_index)
{
  vector <Item*> *removed_stems = 0;
  removed_stems = new vector <Item*>;

  for (vsize j=split_index + 1; j < stems_->size (); j++)
    removed_stems->push_back ((*stems_).at (j));
  for (vsize j=split_index + 1; j < stems_->size (); )
    stems_->pop_back ();
  return (removed_stems);
}

void
Auto_beam_engraver::process_acknowledged ()
{
  Moment now = now_mom();
  if (extend_mom_ > now)
    return;

  if (!process_acknowledged_count_)
    {
      Moment measure_now = measure_position (context ());
      consider_end (measure_now, shortest_mom_);
      consider_begin (measure_now, shortest_mom_);
    }
  else if (process_acknowledged_count_ > 1)
    {
      if (stems_)
	{
	  if ((extend_mom_ < now)
	      || ((extend_mom_ == now) && (last_add_mom_ != now)))
	    end_beam ();
	  else if (!stems_->size ())
	    junk_beam ();
	}
    }

  process_acknowledged_count_++;
}

ADD_ACKNOWLEDGER (Auto_beam_engraver, stem);
ADD_ACKNOWLEDGER (Auto_beam_engraver, bar_line);
ADD_ACKNOWLEDGER (Auto_beam_engraver, beam);
ADD_ACKNOWLEDGER (Auto_beam_engraver, rest);
ADD_TRANSLATOR (Auto_beam_engraver,
		/* doc */
		"Generate beams based on measure characteristics and observed"
		" Stems.  Uses @code{beatLength}, @code{measureLength}, and"
		" @code{measurePosition} to decide when to start and stop a"
		" beam.  Overriding beaming is done through"
		" @ref{Stem_engraver} properties @code{stemLeftBeamCount} and"
		" @code{stemRightBeamCount}.",

		/* create */
		"Beam ",

		/* read */
		"autoBeaming "
		"beamSettings "
		"beatLength "
		"subdivideBeams ",

		/* write */
		""
		);
