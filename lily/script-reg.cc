/*
  script-reg.cc -- implement Script_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "script-reg.hh"
#include "script.hh"
#include "musical-request.hh"
#include "complex-walker.hh"
#include "stem.hh"
#include "staff-sym.hh"

Script_register::Script_register()
{
    post_move_processing();
}

bool
Script_register::try_request(Request *r_l)
{
    if (!r_l->script())
	return false ;
    for (int i=0; i < script_req_l_arr_.size(); i++)
	if ( !Script_req::compare(*script_req_l_arr_[i], *r_l->script())) {
	    return true;
	}
	
    script_req_l_arr_.push( r_l->script());
    
    return true;
}

void
Script_register::process_requests()
{
    for (int i=0; i < script_req_l_arr_.size(); i++){
	Script_req* l=script_req_l_arr_[i];
	Script *p =new Script( l);
	script_p_arr_.push(p);
	announce_element(Score_elem_info(p, l));
    }
}

bool
Script_register::acceptable_elem_b(Score_elem*s_l)
{
    char const *nC = s_l->name();
    return (nC == Stem::static_name());
}

void
Script_register::acknowledge_element(Score_elem_info info)
{
    Score_elem *elem_l = info.elem_l_;
    if (!acceptable_elem_b(elem_l))
	return;
    
    for (int i=0; i < script_p_arr_.size(); i++) {
	Script*script_l = script_p_arr_[i];
	if (elem_l->name() == Stem::static_name())
	    script_l->set_stem((Stem*)elem_l->item());
    }
}

void
Script_register::pre_move_processing()
{
    Staff_symbol* s_l = get_staff_info().staff_sym_l_;
    for (int i=0; i < script_p_arr_.size(); i++) {
	
	Script*script_p = script_p_arr_[i];
	script_p->set_staffsym( s_l);
	typeset_element(script_p);
    }
    script_p_arr_.set_size(0);
}

void
Script_register::post_move_processing()
{
    script_req_l_arr_.set_size(0);
}

IMPLEMENT_STATIC_NAME(Script_register);
ADD_THIS_REGISTER(Script_register);
