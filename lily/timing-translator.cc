/*
  timing-translator.cc -- implement Timing_translator


  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "warn.hh"
#include "timing-translator.hh"

#include "translator-group.hh"
#include "global-context.hh"
#include "multi-measure-rest.hh"


void
Timing_translator::stop_translation_timestep ()
{
  Global_context *global = get_global_context ();

  /* allbars == ! skipbars */
  SCM sb = get_property ("skipBars");
  bool allbars = !to_boolean (sb);

  // urg: multi bar rests: should always process whole of first bar?
  SCM tim = get_property ("timing");
  bool timb = to_boolean (tim);
  if (timb && allbars)
    {
      Moment barleft = (measure_length () - measure_position ());
      Moment now = now_mom ();

      if (barleft > Moment (0)
	  /*
	    Hmm. We insert the bar moment every time we process a
	    moment.  A waste of cpu?
	   */
	  && !now.grace_part_)
	global->add_moment_to_process (now + barleft);
    }
}

void
Timing_translator::initialize ()
{

  /*
    move this to engraver-init.ly? 
   */
  daddy_context_->add_alias (ly_symbol2scm ("Timing"));
  daddy_context_->set_property ("timing" , SCM_BOOL_T);  
  daddy_context_->set_property ("currentBarNumber" , scm_int2num (1));

  daddy_context_->set_property ("timeSignatureFraction",
				scm_cons (scm_int2num (4), scm_int2num (4)));
  /*
    Do not init measurePosition; this should be done from global
    context.
   */
  daddy_context_->set_property ("measureLength", Moment (Rational (1)).smobbed_copy ());
  daddy_context_->set_property ("beatLength", Moment (Rational (1,4)).smobbed_copy ());
}

Rational
Timing_translator::measure_length () const
{
  SCM l = get_property ("measureLength");
  if (unsmob_moment (l))
    return unsmob_moment (l)->main_part_;
  else
    return Rational (1);
}

Timing_translator::Timing_translator ()
{

}

Moment
Timing_translator::measure_position () const
{
  SCM sm = get_property ("measurePosition");
  
  Moment m   =0;
  if (unsmob_moment (sm))
    {
      m = *unsmob_moment (sm);
      while (m.main_part_ < Rational (0))
	m.main_part_ += measure_length ();
    }
  
  return m;
}

void
Timing_translator::start_translation_timestep ()
{
  Global_context *global =get_global_context ();

  Moment now = global->now_mom ();
  Moment dt = now  - global->previous_moment ();
  if (dt < Moment (0))
    {
      programming_error ("Moving backwards in time");
      dt = 0;
    }
  else if (dt.main_part_.is_infinity ())
    {
      programming_error ("Moving infinitely to future");
      dt = 0;
    }
  
  if (!dt.to_bool ())
    return;

  Moment measposp;

  SCM s = get_property ("measurePosition");
  if (unsmob_moment (s))
    {
      measposp = *unsmob_moment (s);
    }
  else
    {
      measposp = now;
      daddy_context_->set_property ("measurePosition",
				    measposp.smobbed_copy ());
    }
  
  measposp += dt;
  
  SCM barn = get_property ("currentBarNumber");
  int b = 0;
  if (is_number (barn))
    {
      b = ly_scm2int (barn);
    }

  SCM cad = get_property ("timing");
  bool c= to_boolean (cad);

  Rational len = measure_length ();
  while (c && measposp.main_part_ >= len)
    {
      measposp.main_part_ -= len;
      b ++;
    }

  daddy_context_->set_property ("currentBarNumber", scm_int2num (b));
  daddy_context_->set_property ("measurePosition", measposp.smobbed_copy ());
}

ENTER_DESCRIPTION (Timing_translator,
		   "This engraver adds the alias "
		   "@code{Timing} to its containing context."
		   ,

		   "","","","","");
