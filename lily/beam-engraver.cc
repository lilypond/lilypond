/*   
  beam-engraver.cc --  implement Beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "timing-engraver.hh"
#include "engraver-group-engraver.hh"
#include "beam-engraver.hh"
#include "musical-request.hh"
#include "beam.hh"
#include "stem.hh"
#include "warn.hh"
#include "time-description.hh"
#include "beaming.hh"
#include "score-engraver.hh"

Beam_engraver::Beam_engraver ()
{
  beam_p_ = 0;
  finished_beam_p_ =0;
  finished_beam_info_p_=0;
  beam_info_p_ =0;
  reqs_drul_[LEFT] = reqs_drul_[RIGHT] =0;
  prev_start_req_ =0;
}

bool
Beam_engraver::do_try_music (Music *m)
{
  if (Span_req * c = dynamic_cast<Span_req*>(m))
    {
      if (c->span_type_str_ != "beam")
	return false;
      
      Direction d =c->span_dir_;

      if (d == STOP && !beam_p_)
	{
	  m->warning (_ ("no beam to end"));
	  return false;
	}
      reqs_drul_[d ] = c;
      return true;
    }
  return false;
}


void
Beam_engraver::do_process_requests ()
{
  if (reqs_drul_[STOP])
    {
      if (!beam_p_)
	reqs_drul_[STOP]->warning (_("no beam to end"));
      prev_start_req_ =0;
      finished_beam_p_ = beam_p_;
      finished_beam_info_p_ = beam_info_p_;

      beam_info_p_ =0;
      beam_p_ = 0;
    }


  if (beam_p_)
    {
      Score_engraver * e = 0;
      Translator * t  =  daddy_grav_l ();
      for (; !e && t;  t = t->daddy_trans_l_)
	{
	  e = dynamic_cast<Score_engraver*> (t);
	}
      
      if (!e)
	programming_error ("No score engraver!");
      else
	e->forbid_breaks ();
    }
  
  if (reqs_drul_[START])
    {
      if (beam_p_)
	{
	  reqs_drul_[START]->warning (_ ("Already have a Beam"));
	  return;
	}

      prev_start_req_ = reqs_drul_[START];
      beam_p_ = new Beam;

      Translator * t  = daddy_grav_l  ()->get_simple_translator ("Timing_engraver");
      Timing_engraver *timer = dynamic_cast<Timing_engraver*> (t);
      beam_start_location_ = (t) ?  timer->time_.whole_in_measure_ : Moment (0);
      beam_start_mom_ = now_mom();
      beam_info_p_ = new Beaming_info_list;
      
      
      /* urg, must copy to Auto_beam_engraver too */
 
      announce_element (Score_element_info (beam_p_, reqs_drul_[START]));
    }
}

void
Beam_engraver::typeset_beam ()
{
  if (finished_beam_p_)
    {
      finished_beam_info_p_->beamify ();
      
      finished_beam_p_->set_beaming (finished_beam_info_p_);
      typeset_element (finished_beam_p_);
      delete finished_beam_info_p_;
      finished_beam_info_p_ =0;
      finished_beam_p_ = 0;
    
      reqs_drul_[STOP] = 0;
    }
}

void
Beam_engraver::do_post_move_processing ()
{
  reqs_drul_ [START] =0;
}

void
Beam_engraver::do_pre_move_processing ()
{
  typeset_beam ();
}

void
Beam_engraver::do_removal_processing ()
{
  typeset_beam ();
  if (beam_p_)
    {
      prev_start_req_->warning (_ ("unfinished beam"));
      finished_beam_p_ = beam_p_;
      finished_beam_info_p_ = beam_info_p_;
      typeset_beam ();
    }
}

void
Beam_engraver::acknowledge_element (Score_element_info info)
{
  if (beam_p_)
    {
      Stem* stem_l = dynamic_cast<Stem *> (info.elem_l_);
      if (!stem_l || stem_l->beam_l_)
	return;


      bool stem_grace = stem_l->get_elt_property (grace_scm_sym) != SCM_BOOL_F;

      SCM wg =get_property ("weAreGraceContext",0);
      bool wgb= gh_boolean_p (wg) && gh_scm2bool (wg);

      if (wgb!= stem_grace)
 	return;

      Rhythmic_req *rhythmic_req = dynamic_cast <Rhythmic_req *> (info.req_l_);
      if (!rhythmic_req)
	{
	  String s = _ ("Stem must have Rhythmic structure");
	  if (info.req_l_)
	    info.req_l_->warning (s);
	  else
	    ::warning (s);
	  
	  return;
	}
      
      if (rhythmic_req->duration_.durlog_i_<= 2)
	{
	  rhythmic_req->warning (_ ("Stem doesn't fit in beam"));
	  prev_start_req_->warning (_ ("Beam was started here"));
	  return;
	}

      stem_l->flag_i_ = rhythmic_req->duration_.durlog_i_;
      Moment stem_location = now_mom () - beam_start_mom_ + beam_start_location_;
      beam_info_p_->add_stem (stem_location, rhythmic_req->duration_.durlog_i_ - 2);
      beam_p_->add_stem (stem_l);
    }
}



ADD_THIS_TRANSLATOR(Beam_engraver);
