/*
  dynamic-reg.cc -- implement Dynamic_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "dynamic-reg.hh"
#include "musical-request.hh"
#include "text-item.hh"
#include "lookup.hh"
#include "paper-def.hh"

Dynamic_register::Dynamic_register()
{
    dir_i_ =0;
    post_move_processing();
    dynamic_p_ =0;
}

void
Dynamic_register::post_move_processing()
{
    dynamic_req_l_ = 0;
}

bool    
Dynamic_register::try_request(Request * r)
{
    Musical_req * m = r->musical();
    if (!m || !m->dynamic())
	return false;
    assert(!dynamic_req_l_);
    dynamic_req_l_ = m->dynamic();
    return true;
}
void
Dynamic_register::process_requests()
{
    if(dynamic_req_l_){
	if (dynamic_req_l_->absdynamic()) {
	    Text_def * td_p = new Text_def;
	    td_p->align_i_ = 0;
	    String loud =Dynamic_req::loudness_str(
		dynamic_req_l_->absdynamic()->loudness_);
	    
	    td_p->text_str_ = paper()->lookup_l()->dynamic(loud).tex;
	    

	    td_p->style_str_ = "dynamic";
	    
	    dynamic_p_ = new Text_item(td_p, 10 ); // TODO!
	    announce_element(Staff_elem_info(dynamic_p_, dynamic_req_l_));
	}
    }
}

void
Dynamic_register::pre_move_processing()
{
    if (dynamic_p_) {
	typeset_element(dynamic_p_);
	dynamic_p_ = 0;
    }
}

bool
Dynamic_register::acceptable_request_b(Request*r)const
{
    Musical_req * m = r->musical();
    return  (m && m->dynamic());
}

void
Dynamic_register::set_feature(Features i)
{
    dir_i_ = i.direction_i_;
}
