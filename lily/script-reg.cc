/*
  script-reg.cc -- implement Script_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "script-reg.hh"
#include "script.hh"
#include "musicalrequest.hh"
#include "complex-walker.hh"
#include "stem.hh"

Script_register::Script_register()
{
    script_p_ = 0;
    post_move_processing();
}

bool
Script_register::try_request(Request *r_l)
{
    if (!r_l->script())
	return false ;

    if (script_req_l_
	&& Script_req::compare(*script_req_l_, *r_l->script()))
	
	return false;

    script_req_l_ = r_l->script();
    
    return true;
}

void
Script_register::process_requests()
{
    if (script_req_l_) {
	script_p_ = new Script(script_req_l_, 10);
	announce_element(
	    Staff_elem_info(script_p_, script_req_l_));
    }
}

void
Script_register::acknowledge_element(Staff_elem_info info)
{
    if (!script_p_)
	return;
    if (info.elem_p_->name() == Stem::static_name())
	script_p_->set_stem((Stem*)info.elem_p_);
    else if (info.req_l_->rhythmic())
	script_p_->set_support(info.elem_p_->item());
}

void
Script_register::pre_move_processing()
{
    if (script_p_){
	script_p_->dir = dir_i_;
	typeset_element(script_p_);
	script_p_ = 0;
    }
}
void
Script_register::post_move_processing()
{
    script_req_l_ = 0;
}

void
Script_register::set_feature(Features i)
{
    if (i.direction_i_|| i.initialiser_b_)
	dir_i_ = i.direction_i_;
}
