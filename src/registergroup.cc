/*
  registergroup.cc -- implement Register_group

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "registergroup.hh"
#include "register.hh"

bool
Register_group::acceptable_request_b(Request* r)
{
    for (iter_top(reg_list_, i); i.ok(); i++) {
	if (i->acceptable_request_b(r))
	    return true;
    }
    return false;
}

void
Register_group::set_dir(int d)
{
    for (iter_top(reg_list_, i); i.ok(); i++) {
	i->set_dir(d);
    }
}

void
Register_group::pre_move_processing()
{
    for (iter_top(reg_list_, i); i.ok(); i++) 
	i->pre_move_processing();
}

void
Register_group::process_requests()
{
    for (iter_top(reg_list_, i); i.ok(); i++) 
	i->process_request();
}

void
Register_group::post_move_processing()
{
    for (iter_top(reg_list_, i); i.ok(); i++) 
	i->post_move_processing();
}

void
Register_group::acknowledge_element(Staff_elem_info info)
{
    for (iter_top(reg_list_, i); i.ok(); i++) 
	i->acknowledge_element(info);
}


bool
Register_group::contains_b(Request_register* reg_l)
{
     for (iter_top(reg_list_, i); i.ok(); i++) 
	if (i.ptr() == reg_l)
	    return true;
     return false;
}


bool
Register_group::try_request(Request*req_l)
{
    for (iter_top(reg_list_, i); i.ok(); i++) 
	if (i->try_request(req_l))
	    return true;
    return false;
}


			    
void
Register_group::add(Request_register *reg_p)
{
    reg_list_.bottom().add(reg_p);
}

Register_group::~Register_group()
{
}
