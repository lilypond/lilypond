/*
  barreg.cc -- implement Bar_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "barreg.hh"
#include "bar.hh"
#include "commandrequest.hh"
#include "complexwalker.hh"
#include "complexcolumn.hh"
#include "sccol.hh"


Bar_register::Bar_register(Complex_walker*w)
    : Request_register(w)
{
    do_post_move_process();
}

bool
Bar_register::try_request(Request*r_l)
{
    if (!r_l->nonmus()->bar()) 
	return false;

    assert(!bar_req_l_);
    bar_req_l_ = r_l->nonmus()->bar();

    return true;
}

void
Bar_register::process_request()
{

    if (bar_req_l_ ) {
	bar_p_ = new Bar(bar_req_l_->type);
    } else if (!walk_l_->time_.whole_in_measure_) {
	bar_p_ = new Bar("|");
    }
    
    if (bar_p_){
	walk_l_->allow_break();    	
	announce_element(Staff_elem_info(bar_p_, bar_req_l_, this) );
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
Bar_register::do_pre_move_process()
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
Bar_register::do_post_move_process()
{
    bar_req_l_ = 0;
    bar_p_ =0;
}
