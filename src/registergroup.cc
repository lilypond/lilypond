/*
  registergroup.cc -- implement Register_group_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "registergroup.hh"
#include "register.hh"

bool
Register_group_register::acceptable_request_b(Request* r)
{
    for (iter_top(reg_list_, i); i.ok(); i++) {
	if (i->acceptable_request_b(r))
	    return true;
    }
    return false;
}

void
Register_group_register::set_feature(Features d)
{
    for (iter_top(reg_list_, i); i.ok(); i++) {
	i->set_feature(d);
    }
}

void
Register_group_register::pre_move_processing()
{
    for (iter_top(reg_list_, i); i.ok(); i++) 
	i->pre_move_processing();
}

void
Register_group_register::process_requests()
{
    for (iter_top(reg_list_, i); i.ok(); i++) 
	i->process_requests();
}

void
Register_group_register::post_move_processing()
{
    for (iter_top(reg_list_, i); i.ok(); i++) 
	i->post_move_processing();
}

void
Register_group_register::acknowledge_element(Staff_elem_info info)
{
    if (!contains_b(info.origin_reg_l_arr_[0]))
	return;
    
    for (iter_top(reg_list_, i); i.ok(); i++) 
	i->acknowledge_element(info);
}

bool
Register_group_register::contains_b(Request_register* reg_l)
{
    bool parent_b = Request_register::contains_b(reg_l);
    
    if (parent_b)
	return true;
    for (iter_top(reg_list_, j); j.ok(); j++)
	if (j->contains_b(reg_l))
	    return true;
    return false;
}
	


bool
Register_group_register::try_request(Request*req_l)
{
    for (iter_top(reg_list_, i); i.ok(); i++) 
	if (i->try_request(req_l))
	    return true;
    return false;
}

void
Register_group_register::add(Request_register *reg_p)
{
    reg_list_.bottom().add(reg_p);
    reg_p->daddy_reg_l_ = this;
}


Register_group_register::~Register_group_register()
{
    
}

Request_register *
Register_group_register::get_register_p(Request_register*reg_l)
{
    iterator(reg_list_) reg_cur= reg_list_.find(reg_l);
    assert(reg_cur.ok());
    return reg_cur.get_p();
}

void
Register_group_register::terminate_register(Request_register*r_l)
{
    delete get_register_p(r_l);
}

void
Register_group_register::do_print()const
{
#ifndef NPRINT
    for (iter_top(reg_list_, i); i.ok(); i++) 
	i->print();
#endif
}
