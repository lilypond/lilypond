/*
  bar-reg.cc -- implement Bar_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "bar-reg.hh"
#include "bar.hh"
#include "command-request.hh"
#include "score-column.hh"
#include "time-description.hh"

Bar_register::Bar_register()
{
    post_move_processing();
}

bool
Bar_register::try_request(Request*r_l)
{
    Command_req* c_l = r_l->command();
    if (!c_l|| !c_l->bar()) 
	return false;
    Bar_req  * b= c_l->bar();
    if (bar_req_l_ && bar_req_l_->compare(*b))
	return false;
    
    bar_req_l_ = b;

    return true;
}

void
Bar_register::process_requests()
{
    if (bar_req_l_ ) {
	bar_p_ = new Bar(bar_req_l_->type_str_);
    } else if (!get_staff_info().time_C_->whole_in_measure_) {
	bar_p_ = new Bar("|");
    }
    
    if (bar_p_){
	announce_element(Staff_elem_info(bar_p_, bar_req_l_) );
    }
}



void
Bar_register::split_bar(Bar *& pre, Bar * no, Bar * &post)
{
    String s= no->type;
    if (s == ":|:") {
	pre = new Bar(":|");
	post = new Bar("|:");
    }else if (s=="|:") {
	post = new Bar(s);
    } else {
	pre = new Bar(*no);
    }
}

void 
Bar_register::pre_move_processing()
{
      if (bar_p_) {
	  Bar * post_p =0;
	  Bar * pre_p =0;
	  split_bar(pre_p, bar_p_, post_p);
	  
	  typeset_breakable_item(pre_p, bar_p_, post_p);
	  bar_p_ =0;
      }
}

void
Bar_register::post_move_processing()
{
    bar_req_l_ = 0;
    bar_p_ =0;
}

IMPLEMENT_STATIC_NAME(Bar_register);


