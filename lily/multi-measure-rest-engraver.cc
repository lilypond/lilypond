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
  rest_stop_mom_ =0;
  // rest_item_creation_mom_ = 0;
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
	   || rest_start_mom_ != now_moment ())
	 return false;
  
     multi_measure_req_l_ = mr;
     rest_start_mom_ = now_moment ();
     
     rest_stop_mom_ = rest_start_mom_ + multi_measure_req_l_->duration_.length ();
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
      // rest_item_creation_mom_ = time->when_mom ();
      announce_element (Score_element_info (mmrest_p_, multi_measure_req_l_));
      start_measure_i_ = time->bars_i_;
    }
}

void
Multi_measure_rest_engraver::do_pre_move_processing ()
{
  Moment now (now_moment ());
  //urg lily dumps core if i want to let her print all (SkipBars=0) rests...
#if 0
  if (mmrest_p_ && (now >= rest_start_mom_) && (mmrest_p_->column_arr_.size () >= 2))
    {
      typeset_element (mmrest_p_);
    }
#endif
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
  Moment now (now_moment ());

  /*
   when our time's up, calculate the number of bars rest and
   make way for new request
   however, linger around a bit to catch this last column when
   its announced
   */
  if (mmrest_p_ && (now >= rest_stop_mom_)) //&& (!time->whole_in_measure_))
    {
      lastrest_p_ = mmrest_p_;
      lastrest_p_->measures_i_ = time->bars_i_ - start_measure_i_;
      //urg lily dumps core if i want to let her print all (SkipBars=0) rests...
#if 0
      if (lastrest_p_->column_arr_.size () >= 2)
        lastrest_p_ = 0;
#endif
      multi_measure_req_l_ = 0;
      mmrest_p_ = 0;
    }
}

