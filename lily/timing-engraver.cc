/*
  timing-grav.cc -- implement Timing_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "context.hh"
#include "grob-info.hh"
#include "multi-measure-rest.hh"
#include "timing-translator.hh"
#include "engraver.hh"
#include "grob.hh"

/**
  Do time bookkeeping
 */
class Timing_engraver : public Timing_translator, public Engraver
{
protected:
  /* Needed to know whether we're advancing in grace notes, or not. */
  Moment last_moment_;
  
  virtual void start_translation_timestep ();
  virtual void initialize ();
  virtual void stop_translation_timestep ();

public:
  TRANSLATOR_DECLARATIONS (Timing_engraver);
};


Timing_engraver::Timing_engraver ()
{
  last_moment_.main_part_ = Rational (-1);
}


void
Timing_engraver::initialize ()
{
  Timing_translator::initialize ();
  
  SCM which = get_property ("whichBar");
  Moment now = now_mom ();
  
  /* Set the first bar of the score? */
  if (!ly_c_string_p (which))
    which = (now.main_part_ || now.main_part_ == last_moment_.main_part_)
      ? SCM_EOL : scm_makfrom0str ("|");

  context ()->set_property ("whichBar", which);
}


void
Timing_engraver::start_translation_timestep ()
{
  Timing_translator::start_translation_timestep ();

  SCM automatic_bars = get_property ("automaticBars");
  Moment now = now_mom ();
  SCM which = get_property ("whichBar");

  /* Set the first bar of the score? */
  if (!ly_c_string_p (which))
    which = SCM_EOL;

  Moment mp = measure_position ();
  bool start_of_measure = (last_moment_.main_part_ != now.main_part_
			   && !mp.main_part_);

  if (start_of_measure)
    {
      Moment mlen = Moment (measure_length ());
      unsmob_grob (get_property ("currentCommandColumn"))
	->set_property ("measure-length", mlen.smobbed_copy ()); 
    }
  
  if (!ly_c_string_p (which) && to_boolean (automatic_bars))
    {
      SCM always = get_property ("barAlways");

      if ( start_of_measure || (to_boolean (always)))
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


ENTER_DESCRIPTION (Timing_engraver,
/* descr */       " Responsible for synchronizing timing information from staves.  "
"Normally in @code{Score}.  In order to create polyrhythmic music, "
"this engraver should be removed from @code{Score} and placed in "
"@code{Staff}. "
"\n\nThis engraver adds the alias @code{Timing} to its containing context."

		   ,
/* creats*/       "",
/* accepts */     "",
/* acks  */      "",
/* reads */       "timeSignatureFraction automaticBars whichBar barAlways defaultBarType skipBars timing measureLength measurePosition currentBarNumber",
/* write */       "");
