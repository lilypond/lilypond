/*
  timing-engraver.cc -- implement Timing_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "timing-translator.hh"
#include "engraver.hh"

#include "context.hh"
#include "multi-measure-rest.hh"
#include "grob.hh"
#include "warn.hh"


class Timing_engraver : public Timing_translator, public Engraver
{
protected:
  /* Need to know whether we're advancing in grace notes, or not. */
  Moment last_moment_;

  virtual void start_translation_timestep ();
  virtual void process_music ();
  virtual void stop_translation_timestep ();

public:
  TRANSLATOR_DECLARATIONS (Timing_engraver);
};

ADD_TRANSLATOR (Timing_engraver,
		/* descr */ " Responsible for synchronizing timing information from staves.  "
		"Normally in @code{Score}.  In order to create polyrhythmic music, "
		"this engraver should be removed from @code{Score} and placed in "
		"@code{Staff}. "
		"\n\nThis engraver adds the alias @code{Timing} to its containing context.",
		/* creats*/ "",
		/* accepts */ "",
		/* acks  */ "",
		/* reads */ "automaticBars whichBar barAlways defaultBarType "
		"skipBars timing measureLength measurePosition currentBarNumber",
		/* write */ "");


Timing_engraver::Timing_engraver ()
{
  last_moment_.main_part_ = Rational (-1);
}

void
Timing_engraver::process_music ()
{
  Timing_translator::process_music ();

  bool start_of_measure = (last_moment_.main_part_ != now_mom ().main_part_
			   && !measure_position ().main_part_);

  /*
    We can't do this in start_translation_timestep(), since time sig
    changes won't have happened by then.
  */
  if (start_of_measure)
    {
      Moment mlen = Moment (measure_length ());
      Grob * column = unsmob_grob (get_property ("currentCommandColumn"));
      if (column)
	column->set_property ("measure-length", mlen.smobbed_copy ());
      else
	programming_error("No command column?");
    }
}

void
Timing_engraver::start_translation_timestep ()
{
  Timing_translator::start_translation_timestep ();

  SCM automatic_bars = get_property ("automaticBars");
  Moment now = now_mom ();
  SCM which = get_property ("whichBar");

  /* Set the first bar of the score? */
  if (!scm_is_string (which))
    which = SCM_EOL;

  Moment mp = measure_position ();
  bool start_of_measure = (last_moment_.main_part_ != now.main_part_
			   && !mp.main_part_);

  if (!scm_is_string (which) && to_boolean (automatic_bars))
    {
      SCM always = get_property ("barAlways");

      if ((start_of_measure && last_moment_.main_part_ >= Moment (0))
	  || to_boolean (always))
	{
	  /* should this work, or be junked?  See input/bugs/no-bars.ly */
	  which = get_property ("defaultBarType");
	}
    }

  context ()->set_property ("whichBar", which);
}

void
Timing_engraver::stop_translation_timestep ()
{
  Timing_translator::stop_translation_timestep ();
  context ()->set_property ("whichBar", SCM_EOL);
  last_moment_ = now_mom ();
}
