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
Timing_translator::do_try_music (Music*r)
{
  if (Timing_req *t =  dynamic_cast <Timing_req *> (r))
    {
      for (int i=0; i < timing_req_l_arr_.size (); i++)
	{
	  /*
	    merge timing reqs.
	   */
	  if (timing_req_l_arr_[i]->equal_b(t))
	    return true;
	  if (String (classname (timing_req_l_arr_[i])) == classname (r))
	    {
	      r->origin ()->warning (_ ("conflicting timing request"));
	      timing_req_l_arr_[i]->origin ()->warning (_("This is the other timing request")); 
	      return false;
	    }
	}
    
      timing_req_l_arr_.push(t);
      return true;
    }
  return false;
}
void
Timing_translator::do_process_music()
{
  for (int i=0; i < timing_req_l_arr_.size (); i++)
    {
      Timing_req * tr_l = timing_req_l_arr_[i];

      if (Time_signature_change_req *m_l = dynamic_cast <Time_signature_change_req *> (tr_l))
	{
	  int b_i= m_l->beats_i_;
	  int o_i = m_l->one_beat_i_;
	  set_time_signature (b_i, o_i);
	}
      else if (dynamic_cast <Barcheck_req *> (tr_l))
	{
	  if (measure_position ())
	    {
	      Moment nm; 
	      tr_l ->origin ()->warning (_f ("barcheck failed at: %s", 
				  measure_position ().str ()));
	      // resync
	      daddy_trans_l_->set_property("measurePosition", nm.make_scm ());
	    }
	}
    }
}


void
Timing_translator::do_pre_move_processing()
{
  timing_req_l_arr_.set_size (0);
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
Timing_translator::do_creation_processing()
{
  Moment m;
  daddy_trans_l_->set_property ("timing" , SCM_BOOL_T);  
  daddy_trans_l_->set_property ("currentBarNumber" , gh_int2scm (1));
  daddy_trans_l_->set_property ("measurePosition", m.make_scm ());
  daddy_trans_l_->set_property ("beatLength", Moment (1,4).make_scm ());
  daddy_trans_l_->set_property ("measureLength",  Moment (1).make_scm());
  daddy_trans_l_->set_property ("timeSignatureFraction",
				gh_cons (gh_int2scm (4), gh_int2scm (4)));
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
Timing_translator::set_time_signature (int l, int o)
{
  Moment one_beat = Moment (1)/Moment (o);
  Moment len = Moment (l) * one_beat;
  daddy_trans_l_->set_property ("measureLength", len.make_scm ());
  daddy_trans_l_->set_property ("beatLength", one_beat.make_scm ());
  daddy_trans_l_->set_property ("timeSignatureFraction",
				gh_cons (gh_int2scm (l), gh_int2scm (o)));
}

Timing_translator::Timing_translator()
{
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
Timing_translator::do_post_move_processing()
{
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
      daddy_trans_l_->set_property ("measurePosition", measposp.make_scm());
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
  daddy_trans_l_->set_property ("measurePosition", measposp.make_scm());
}

