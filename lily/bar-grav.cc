/*
  bar-reg.cc -- implement Bar_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "bar-grav.hh"
#include "bar.hh"
#include "command-request.hh"
#include "time-description.hh"
#include "engraver-group.hh"

Bar_engraver::Bar_engraver()
{
    do_post_move_processing();
}

bool
Bar_engraver::do_try_request (Request*r_l)
{
    Command_req* c_l = r_l->command();
    if (!c_l|| !c_l->bar()) 
	return false;
    Bar_req  * b= c_l->bar();
    if (bar_req_l_ && bar_req_l_->equal_b (b))
	return false;
    
    bar_req_l_ = b;

    return true;
}

void
Bar_engraver::do_process_requests()
{
    if (bar_req_l_) {
	bar_p_ = new Bar;
	bar_p_->type_str_=bar_req_l_->type_str_;
    } else if (!get_staff_info().time_C_->whole_in_measure_) {
 	bar_p_ = new Bar;
    }
    
    if (bar_p_){
	announce_element (Score_elem_info (bar_p_, bar_req_l_));
    } else {
	Disallow_break_req r;
	daddy_grav_l_->try_request (&r);
    }
}


void 
Bar_engraver::do_pre_move_processing()
{
      if (bar_p_) {
	  typeset_element (bar_p_);
	  bar_p_ =0;
      }
}

void
Bar_engraver::do_post_move_processing()
{
    bar_req_l_ = 0;
    bar_p_ =0;
}


IMPLEMENT_IS_TYPE_B1(Bar_engraver,Engraver);
ADD_THIS_ENGRAVER(Bar_engraver);


