/*
  timing-translator.cc -- implement Timing_translator


  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "timing-translator.hh"
#include "command-request.hh"
#include "translator-group.hh"
#include "global-translator.hh"
#include "multi-measure-rest.hh"


void
Timing_translator::stop_translation_timestep ()
{
  Translator *t = this;
  Global_translator *global_l =0;
  do
    {
      t = t->daddy_trans_l_ ;
      global_l = dynamic_cast<Global_translator*> (t);
    }
  while (!global_l);

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
	global_l->add_moment_to_process (now + barleft);
    }
}

void
Timing_translator::initialize ()
{
  daddy_trans_l_->set_property ("timing" , SCM_BOOL_T);  
  daddy_trans_l_->set_property ("currentBarNumber" , gh_int2scm (1));

  daddy_trans_l_->set_property ("timeSignatureFraction",
				gh_cons (gh_int2scm (4), gh_int2scm (4)));
  daddy_trans_l_->set_property ("measurePosition", Moment (Rational (0)).smobbed_copy ());
  daddy_trans_l_->set_property ("measureLength", Moment (Rational (1)).smobbed_copy ());
  daddy_trans_l_->set_property ("beatLength", Moment (Rational (1,4)).smobbed_copy ());
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
  Translator *t = this;
  Global_translator *global_l =0;
  do
    {
      t = t->daddy_trans_l_ ;
      global_l = dynamic_cast<Global_translator*> (t);
    }
  while (!global_l);

  Moment now = global_l->now_mom_;
  Moment dt = now  - global_l -> prev_mom_;
  if (dt < Moment (0))
    {
      programming_error ("Moving backwards in time");
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
      daddy_trans_l_->set_property ("measurePosition", measposp.smobbed_copy ());
    }
  
  measposp += dt;
  
  SCM barn = get_property ("currentBarNumber");
  int b = 0;
  if (gh_number_p (barn))
    {
      b = gh_scm2int (barn);
    }

  SCM cad = get_property ("timing");
  bool c= to_boolean (cad);

  Rational len = measure_length ();
  while (c && measposp.main_part_ >= len)
    {
      measposp.main_part_ -= len;
      b ++;
    }

  daddy_trans_l_->set_property ("currentBarNumber", gh_int2scm (b));
  daddy_trans_l_->set_property ("measurePosition", measposp.smobbed_copy ());
}

ENTER_DESCRIPTION (Timing_translator, "","","","","" );
