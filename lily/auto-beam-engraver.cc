/*
  auto-beam-engraver.cc -- implement Auto_beam_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "beaming.hh"
#include "beam.hh"
#include "stem.hh"
#include "warn.hh"
#include "bar-line.hh"
#include "rest.hh"
#include "item.hh"
#include "spanner.hh"
#include "context.hh"
#include "duration.hh"

#include "translator.icc"

class Auto_beam_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Auto_beam_engraver);

protected:
  void stop_translation_timestep ();
  void start_translation_timestep ();
  void process_music ();
  virtual bool try_music (Music *);
  virtual void finalize ();
  virtual void derived_mark () const;

  DECLARE_ACKNOWLEDGER (rest);
  DECLARE_ACKNOWLEDGER (beam);
  DECLARE_ACKNOWLEDGER (bar_line);
  DECLARE_ACKNOWLEDGER (stem);

  void process_acknowledged ();

private:
  bool test_moment (Direction, Moment);
  void consider_begin (Moment);
  void consider_end (Moment);
  Spanner *create_beam ();
  void begin_beam ();
  void end_beam ();
  void junk_beam ();
  bool is_same_grace_state (Grob *e);
  void typeset_beam ();

  Music *forbid_;
  /*
    shortest_mom is the shortest note in the beam.
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

  bool subdivide_beams_;
  Moment beat_length_;

  // We act as if beam were created, and start a grouping anyway.
  Beaming_info_list *grouping_;
  SCM beam_settings_;

  Beaming_info_list *finished_grouping_;

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
      consider_end (shortest_mom_);
      junk_beam ();
    }
}

void
Auto_beam_engraver::process_music ()
{
  if (scm_is_string (get_property ("whichBar")))
    {
      consider_end (shortest_mom_);
      junk_beam ();
    }

  if (forbid_)
    {
      consider_end (shortest_mom_);
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

bool
Auto_beam_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("beam-forbid-event"))
    {
      forbid_ = m;
      return true;
    }

  return false;
}

bool
Auto_beam_engraver::test_moment (Direction dir, Moment test)
{
  return scm_call_3 (get_property ("autoBeamCheck"),
		     context ()->self_scm (),
		     scm_from_int (dir),
		     test.smobbed_copy ())
    != SCM_BOOL_F;
}

void
Auto_beam_engraver::consider_begin (Moment test_mom)
{
  bool on = to_boolean (get_property ("autoBeaming"));
  if (!stems_ && on
      && !forbid_)
    {
      bool b = test_moment (START, test_mom);
      if (b)
	begin_beam ();
    }
}

void
Auto_beam_engraver::consider_end (Moment test_mom)
{
  if (stems_)
    {
      /* Allow already started autobeam to end:
	 don't check for autoBeaming */
      bool b = test_moment (STOP, test_mom);
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
    Can't use make_spanner_from_properties() because we have to use
    beam_settings_.
  */
  Spanner *beam = new Spanner (beam_settings_,
			       context ()->get_grob_key ("Beam"));

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
  grouping_ = new Beaming_info_list;
  beam_settings_ = updated_grob_properties (context (), ly_symbol2scm ("Beam"));

  beam_start_moment_ = now_mom ();
  beam_start_location_
    = robust_scm2moment (get_property ("measurePosition"), Moment (0));
  subdivide_beams_ = ly_scm2bool (get_property ("subdivideBeams"));
  beat_length_ = robust_scm2moment (get_property ("beatLength"), Moment (1, 4));
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
	finished_grouping_ = grouping_;
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
      finished_grouping_->beamify (beat_length_, subdivide_beams_);
      Beam::set_beaming (finished_beam_, finished_grouping_);
      finished_beam_ = 0;

      delete finished_grouping_;
      finished_grouping_ = 0;
    }
}

void
Auto_beam_engraver::start_translation_timestep ()
{
  process_acknowledged_count_ = 0;
  /*
    don't beam over skips
  */
  if (stems_)
    {
      Moment now = now_mom ();
      if (extend_mom_ < now)
	end_beam ();
    }
  forbid_ = 0;
}

void
Auto_beam_engraver::stop_translation_timestep ()
{
  typeset_beam ();
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
Auto_beam_engraver::acknowledge_beam (Grob_info info)
{
  (void)info;
  check_bar_property ();
  if (stems_)
    end_beam ();
}

void
Auto_beam_engraver::acknowledge_bar_line (Grob_info info)
{
  (void)info;
  check_bar_property ();
  if (stems_)
    end_beam ();
}

void
Auto_beam_engraver::acknowledge_rest (Grob_info info)
{
  (void)info;
  check_bar_property ();
  if (stems_)
    end_beam ();
}

void
Auto_beam_engraver::acknowledge_stem (Grob_info info)
{
  check_bar_property ();
  Item *stem = dynamic_cast<Item *> (info.grob ());
  Music *m = info.ultimate_music_cause ();
  if (!m->is_mus_type ("rhythmic-event"))
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

  int durlog = unsmob_duration (m->get_property ("duration"))->duration_log ();

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

  Moment dur = unsmob_duration (m->get_property ("duration"))->get_length ();

  consider_end (dur);
  consider_begin (dur);

  if (dur < shortest_mom_)
    shortest_mom_ = dur;

  if (!stems_)
    return;

  grouping_->add_stem (now - beam_start_moment_ + beam_start_location_,
		       durlog - 2);
  stems_->push_back (stem);
  last_add_mom_ = now;
  extend_mom_ = max (extend_mom_, now) + m->get_length ();
}

void
Auto_beam_engraver::process_acknowledged ()
{
  if (extend_mom_ > now_mom ())
    return;

  if (!process_acknowledged_count_)
    {
      consider_end (shortest_mom_);
      consider_begin (shortest_mom_);
    }
  else if (process_acknowledged_count_ > 1)
    {
      if (stems_)
	{
	  Moment now = now_mom ();
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
		/* doc */ "Generate beams based on measure characteristics and observed "
		"Stems.  Uses beatLength, measureLength and measurePosition to decide "
		"when to start and stop a beam.  Overriding beaming is done through "
		"@ref{Stem_engraver} properties @code{stemLeftBeamCount} and "
		"@code{stemRightBeamCount}. ",
		/* create */ "Beam",
		/* accept */ "beam-forbid-event",
		/* read */ "autoBeaming autoBeamSettings beatLength subdivideBeams",
		/* write */ "");
