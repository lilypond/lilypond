/*
  multi_measure_rest-engraver.cc -- implement Multi_measure_rest_engraver

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
       Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"
#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "multi-measure-rest-engraver.hh"
#include "score-column.hh"
#include "time-description.hh"
//#include "paper-score.hh"
//#include "p-score.hh"
//#include "paper-def.hh"
//#include "main.hh"
//#include "global-translator.hh"
#include "bar.hh"


ADD_THIS_TRANSLATOR (Multi_measure_rest_engraver);

Multi_measure_rest_engraver::Multi_measure_rest_engraver ()
{
  start_measure_i_ = 0;
  rest_moments_[START] =
    rest_moments_[STOP] =0;
  multi_measure_req_l_ = 0;
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
 if (Multi_measure_rest_req *mr = dynamic_cast<Multi_measure_rest_req *> (req_l))
   {
     if (multi_measure_req_l_)
       if (!multi_measure_req_l_->equal_b (mr)
	   || rest_moments_[START] != now_mom ())
	 return false;
  
     multi_measure_req_l_ = mr;
     rest_moments_[START] = now_mom ();
     
     rest_moments_[STOP] = rest_moments_[START] + multi_measure_req_l_->duration_.length_mom ();
     return true;
   }
 return false;
}

void
Multi_measure_rest_engraver::do_process_requests ()
{
  if (multi_measure_req_l_ && !mmrest_p_)
    {
      Time_description const *time = get_staff_info().time_C_;
      mmrest_p_ = new Multi_measure_rest;
      announce_element (Score_element_info (mmrest_p_, multi_measure_req_l_));
      start_measure_i_ = time->bars_i_;
    }
}

void
Multi_measure_rest_engraver::do_pre_move_processing ()
{
  Moment now (now_mom ());
  Time_description const *time = get_staff_info().time_C_;
  if (mmrest_p_ && (now >= rest_moments_[START]) 
    && !time->whole_in_measure_
    && (mmrest_p_->column_arr_.size () >= 2))
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
  Time_description const *time = get_staff_info().time_C_;
  Moment now (now_mom ());

  if (mmrest_p_ && !time->whole_in_measure_)
    {
      lastrest_p_ = mmrest_p_;
      lastrest_p_->measures_i_ = time->bars_i_ - start_measure_i_;
      mmrest_p_ = 0;
    }

  if (now >= rest_moments_[STOP])
    {
      multi_measure_req_l_ = 0;
      mmrest_p_ = 0;
    }
}

