/*
  register.cc -- implement Request_register

  Sourcefile of GNU LilyPond musictypesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "music-list.hh"
#include "musical-request.hh"
#include "register.hh"
#include "register-group.hh"
#include "debug.hh"

void
Request_register::post_move_processing()
{
        
    if (status < CREATION_INITED) {
	do_creation_processing();
	status = CREATION_INITED;
    }
    if (status >= MOVE_INITED)
	return;

    do_post_move_processing();
    status = MOVE_INITED;
}

bool
Request_register::try_request(Request * r)
{
    if (status < MOVE_INITED)
	post_move_processing();

    return do_try_request(r);
}

void
Request_register::process_requests()
{
    if (status < PROCESSED_REQS)
	post_move_processing();
    else if (status >= PROCESSED_REQS)
	return; 
    
    status = PROCESSED_REQS;
    do_process_requests();
}

void
Request_register::pre_move_processing()
{
    do_pre_move_processing();
    status = CREATION_INITED;
}

void
Request_register::fill_staff_info(Staff_info&)
{
    
}

Scalar
Request_register::get_feature(String t)
{
    return daddy_reg_l_->get_feature(t);
}

bool
Request_register::do_try_request(Request*)
{
    return false;
}

Request_register::Request_register()
{
    status = VIRGIN;
    daddy_reg_l_ = 0;
}

void
Request_register::announce_element(Score_elem_info i)
{
    i.origin_reg_l_arr_.push(this);
    daddy_reg_l_->announce_element(i);
}

void
Request_register::typeset_element(Score_elem*p)
{
    daddy_reg_l_->typeset_element(p);
}

Paper_def*
Request_register::paper()const
{
    return daddy_reg_l_->paper();
}

void
Request_register::typeset_breakable_item(Item * nobreak_p)
{
    daddy_reg_l_->typeset_breakable_item(nobreak_p);
}

bool
Request_register::contains_b(Request_register *reg_l)const
{
    return this == reg_l;
}

Staff_info
Request_register::get_staff_info() const
{
    return daddy_reg_l_->get_staff_info();
}

void
Request_register::print() const
{
#ifndef NPRINT
    mtor << "\n" << name() << " {";
    do_print();
    mtor << "}";
#endif
}

IMPLEMENT_STATIC_NAME(Request_register);
IMPLEMENT_IS_TYPE_B(Request_register);

void
Request_register::do_print()const
{
}


