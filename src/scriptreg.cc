/*
  scriptreg.cc -- implement Script_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "scriptreg.hh"
#include "script.hh"
#include "request.hh"
#include "complexwalker.hh"

Script_register::Script_register(Complex_walker*w)
    : Request_register(w)
{
    script_p_ = 0;
}

bool
Script_register::try_request(Request *r_l)
{
    if (!r_l->script())
	return false ;

    if (accepted_req_arr_.size()
	&& Script_req::compare(*accepted_req_arr_[0]->script(), *r_l->script()))
	
	return false;

    accepted_req_arr_.push(r_l);
    
    return true;
}

void
Script_register::process_request()
{
    if (accepted_req_arr_.size() ) {
	script_p_ = new Script(accepted_req_arr_[0]->script(), 10);
	announce_element(
	    Staff_elem_info(script_p_, accepted_req_arr_[0], this));
    }
}

void
Script_register::acknowledge_element(Staff_elem_info info)
{
    if (!script_p_)
	return;
    if (info.elem_p_->name() == String("Stem"))
	script_p_->set_stem((Stem*)info.elem_p_);
    else if (info.req_l_->rhythmic())
	script_p_->set_support(info.elem_p_->item());
}

void
Script_register::do_pre_move_process()
{
    if (script_p_){
	script_p_->dir = dir_i_;
	typeset_element(script_p_);
	script_p_ = 0;
    }
}

void
Script_register::set_dir(int i)
{
    dir_i_ = i;
}
