/*
  timing-translator.cc -- implement Timing_translator


  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "timing-translator.hh"
#include "command-request.hh"
#include "translator-group.hh"
#include "global-translator.hh"
#include "multi-measure-rest.hh"

/*
  TODO: change the rest of lily, so communication with
  Timing_translator is only done through properties.  This means the
  class declaration can go here.  */

bool
Timing_translator::try_music (Music*r)
{
  if (dynamic_cast<Barcheck_req*> (r))
    {
      check_ = r;
      return true;
    }
  return false;
}

void
Timing_translator::process_music()
{
  if (check_ && measure_position ())
    {
      check_->origin ()->warning (_f ("barcheck failed at: %s", 
				      measure_position ().str ()));
      Moment zero; 
      
      if (!to_boolean (get_property ("barCheckNoSynchronize")))
	daddy_trans_l_->set_property("measurePosition", zero.smobbed_copy ());
    }

  SCM fr = get_property ("timeSignatureFraction");
  
  if (scm_equal_p (fr, last_time_sig_) == SCM_BOOL_F)
    {
      last_time_sig_ = fr;
      set_time_signature ();
    }
}


void
Timing_translator::stop_translation_timestep()
{
  check_ = 0;
  
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

      if (barleft > Moment (0))
	global_l->add_moment_to_process (now_mom () + barleft);
    }
}


ADD_THIS_TRANSLATOR(Timing_translator);

void
Timing_translator::initialize()
{
  Moment m;
  daddy_trans_l_->set_property ("timing" , SCM_BOOL_T);  
  daddy_trans_l_->set_property ("currentBarNumber" , gh_int2scm (1));
  daddy_trans_l_->set_property ("measurePosition", m.smobbed_copy ());
  daddy_trans_l_->set_property ("timeSignatureFraction",
				gh_cons (gh_int2scm (4), gh_int2scm (4)));

  set_time_signature ();
}

Moment
Timing_translator::measure_length () const
{
  SCM l = get_property("measureLength");
  if (unsmob_moment(l))
    return *unsmob_moment (l);
  else
    return Moment (1);
}


void
Timing_translator::set_time_signature ()
{
  SCM fr = get_property ("timeSignatureFraction");
  int l = gh_scm2int (gh_car (fr));
  int o = gh_scm2int (gh_cdr (fr));
  
  Moment one_beat = Moment (1)/Moment (o);
  Moment len = Moment (l) * one_beat;

  daddy_trans_l_->set_property ("measureLength", len.smobbed_copy ());
  daddy_trans_l_->set_property ("beatLength", one_beat.smobbed_copy ());
}

Timing_translator::Timing_translator()
{

  last_time_sig_ = SCM_BOOL_F;

}


Moment
Timing_translator::measure_position () const
{
  SCM sm = get_property ("measurePosition");
  
  Moment m   =0;
  if (unsmob_moment (sm))
    {
      m = *unsmob_moment(sm);
      while (m < Moment (0))
	m += measure_length ();
    }
  
  return m;
}

void
Timing_translator::start_translation_timestep()
{
	check_ =00;
  Translator *t = this;
  Global_translator *global_l =0;
  do
    {
      t = t->daddy_trans_l_ ;
      global_l = dynamic_cast<Global_translator*> (t);
    }
  while (!global_l);

  Moment dt = global_l->now_mom_  - global_l -> prev_mom_;
  if (dt < Moment (0))
    {
      programming_error ("Moving backwards in time");
      dt = 0;
    }
  
  if (!dt)
    return;

  Moment measposp;

  SCM s = get_property ("measurePosition");
  if (unsmob_moment (s))
    {
      measposp = *unsmob_moment(s);
    }
  else
    {
      daddy_trans_l_->set_property ("measurePosition", measposp.smobbed_copy());
    }
  
  measposp += dt;
  
  SCM barn = get_property ("currentBarNumber");
  int b = 0;
  if (gh_number_p(barn))
    {
      b = gh_scm2int (barn);
    }

  SCM cad = get_property ("timing");
  bool c= to_boolean (cad );

  Moment len = measure_length ();
  while (c && measposp >= len)
    {
      measposp -= len;
      b ++;
    }

  daddy_trans_l_->set_property ("currentBarNumber", gh_int2scm (b));
  daddy_trans_l_->set_property ("measurePosition", measposp.smobbed_copy());
}

