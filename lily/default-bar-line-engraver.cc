/*
  timing-engraver.cc -- implement Default_bar_line_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "context.hh"
#include "multi-measure-rest.hh"
#include "grob.hh"
#include "warn.hh"

class Default_bar_line_engraver : public Engraver
{
protected:
  /* Need to know whether we're advancing in grace notes, or not. */
  Moment last_moment_;

  void start_translation_timestep ();
  void stop_translation_timestep ();

public:
  TRANSLATOR_DECLARATIONS (Default_bar_line_engraver);
};

#include "translator.icc"

ADD_TRANSLATOR (Default_bar_line_engraver,
		/* doc */
		"This engraver determines what kind of automatic bar lines"
		" should be produced, and sets @code{whichBar} accordingly."
		"  It should be at the same level as @ref{Timing_translator}.",
		
		/* create */
		"",

		/* read */
		"automaticBars "
		"barAlways "
		"defaultBarType "
		"measureLength "
		"whichBar "
		"measurePosition ",
		
		/* write */
		"automaticBars "
		);

Default_bar_line_engraver::Default_bar_line_engraver ()
{
  last_moment_.main_part_ = Rational (-1);
}

void
Default_bar_line_engraver::start_translation_timestep ()
{
  SCM automatic_bars = get_property ("automaticBars");
  Moment now = now_mom ();
  SCM which = get_property ("whichBar");

  /* Set the first bar of the score? */
  if (!scm_is_string (which))
    which = SCM_EOL;

  Moment mp = measure_position (context ());
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
Default_bar_line_engraver::stop_translation_timestep ()
{
  context ()->set_property ("whichBar", SCM_EOL);
  last_moment_ = now_mom ();
}
