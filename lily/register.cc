/*
  register.cc -- implement Request_register

  Sourcefile of LilyPond musictypesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "voice.hh"
#include "musicalrequest.hh"
#include "register.hh"
#include "notehead.hh"
#include "complex-walker.hh"
#include "local-key-item.hh"
#include "complex-staff.hh"
#include "registergroup.hh"
#include "debug.hh"


bool
Request_register::try_request(Request*)
{
    return false;
}

Request_register::Request_register()
{
    daddy_reg_l_ = 0;
}

void
Request_register::announce_element(Staff_elem_info i)
{
    i.origin_reg_l_arr_.push(this);
    daddy_reg_l_->announce_element(i);
}

void
Request_register::typeset_element(Staff_elem*p)
{
    daddy_reg_l_->typeset_element(p);
}

Paper_def*
Request_register::paper()const
{
    return daddy_reg_l_->paper();
}

void
Request_register::typeset_breakable_item(Item * pre_p , Item * nobreak_p,
					 Item * post_p)
{
    daddy_reg_l_->typeset_breakable_item(pre_p,  nobreak_p, post_p);
}

bool
Request_register::acceptable_request_b(Request*)const
{
    return false;
}

bool
Request_register::contains_b(Request_register *reg_l)
{
    return this == reg_l;
}

Staff_info
Request_register::get_staff_info() return inf;
{
    inf = daddy_reg_l_->get_staff_info();
}

void
Request_register::print() const
{
#ifndef NPRINT
    mtor << name() << " {";
    do_print();
    mtor << "}\n";
#endif
}

void
Request_register::do_print()const
{
}
