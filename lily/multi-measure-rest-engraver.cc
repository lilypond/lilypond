/*
  multi_measure_rest-engraver.cc -- implement Multi_measure_rest_engraver

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
       Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"
#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "multi-measure-rest-engraver.hh"
#include "paper-column.hh"
#include "engraver-group-engraver.hh"
#include "timing-translator.hh"
#include "bar.hh"
#include "staff-symbol-referencer.hh"


ADD_THIS_TRANSLATOR (Multi_measure_rest_engraver);

Multi_measure_rest_engraver::Multi_measure_rest_engraver ()
{
  start_measure_i_ = 0;
  rest_moments_[START] =
    rest_moments_[STOP] =0;
  multi_measure_req_l_ = 0;

  lastrest_p_ =0;
  mmrest_p_ = 0;
}

void
Multi_measure_rest_engraver::acknowledge_element (Score_element_info i)
{
  if (Bar *c = dynamic_cast<Bar*> (i.elem_l_))
    {
      if (mmrest_p_) 
	mmrest_p_->add_column (c);
      if (lastrest_p_)
	lastrest_p_->add_column (c);
    }
}

bool
Multi_measure_rest_engraver::do_try_music (Music* req_l)
{
  Rhythmic_req *r=0;
  if (Multi_measure_rest_req *mr = 
      dynamic_cast<Multi_measure_rest_req *> (req_l))
    r=mr;
  else if (Repetitions_req *rr = 
	   dynamic_cast<Repetitions_req *> (req_l))
    r=rr;
  if (r)
    {
      if (multi_measure_req_l_)
	if (!multi_measure_req_l_->equal_b (r)
	    || rest_moments_[START] != now_mom ())
	  return false;
      
      multi_measure_req_l_ = r;
      rest_moments_[START] = now_mom ();
      
      rest_moments_[STOP] = rest_moments_[START] +
	multi_measure_req_l_->duration_.length_mom ();
      return true;
    }
  return false;
}

void
Multi_measure_rest_engraver::do_process_requests ()
{
  if (multi_measure_req_l_ && !mmrest_p_)
    {

      Translator * tr = daddy_grav_l()->get_simple_translator ("Timing_engraver");	// ugh
      Timing_translator * time = dynamic_cast<Timing_translator*> (tr);

      mmrest_p_ = new Multi_measure_rest;
      Staff_symbol_referencer_interface si (mmrest_p_);
      si.set_interface ();

      
      if(dynamic_cast<Repetitions_req *> (multi_measure_req_l_))
	mmrest_p_->set_elt_property ("alt-symbol", 
				     ly_str02scm ("scripts-repeatsign"));
      announce_element (Score_element_info (mmrest_p_, multi_measure_req_l_));
      start_measure_i_ = time->bars_i ();
    }
}

void
Multi_measure_rest_engraver::do_pre_move_processing ()
{
  Moment now (now_mom ());
  Translator * tr = daddy_grav_l()->get_simple_translator ("Timing_engraver");	// ugh
  Timing_translator * time  = dynamic_cast<Timing_translator*> (tr);


  if (mmrest_p_ && (now >= rest_moments_[START]) 
    && !time->measure_position ()
    && (scm_ilength (mmrest_p_->get_elt_property ("columns")) >= 2))
    {
      typeset_element (mmrest_p_);
      /*
	must keep mmrest_p_ around to set measures_i_
       */
    }
  if (lastrest_p_)
    {
      typeset_element (lastrest_p_);
      lastrest_p_ = 0;
    }
}

void
Multi_measure_rest_engraver::do_post_move_processing ()
{
  Translator * tr = daddy_grav_l()->get_simple_translator ("Timing_engraver");	// ugh
  Timing_translator * time  = dynamic_cast<Timing_translator*> (tr);

  Moment now (now_mom ());

  if (mmrest_p_ && !time->measure_position ())
    {
      lastrest_p_ = mmrest_p_;
      lastrest_p_->measures_i_ = time->bars_i () - start_measure_i_;
      mmrest_p_ = 0;
    }

  if (now >= rest_moments_[STOP])
    {
      multi_measure_req_l_ = 0;
      mmrest_p_ = 0;
    }
}


