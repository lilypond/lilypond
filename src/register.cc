/*
  register.cc -- implement  Staff_elem_info, Request_register

  Sourcefile of LilyPond musictypesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "voice.hh"
#include "request.hh"
#include "register.hh"
#include "notehead.hh"
#include "complexwalker.hh"
#include "localkeyitem.hh"
#include "complexstaff.hh"

Staff_elem_info::Staff_elem_info(Staff_elem*s_l, Request*r_l,
				 Request_register *reg_l)
{
    elem_p_ = s_l;
    voice_l_ = r_l->elt_l_->voice_l_;
    req_l_ = r_l;
    group_regs_l_ = 0;
    origin_reg_l_ = reg_l;
}

Staff_elem_info::Staff_elem_info()
{
    elem_p_ = 0;
    voice_l_ = 0;

    group_regs_l_ = 0;
    origin_reg_l_ = 0;
    req_l_ = 0;
}

/* *************** */

Request_register::Request_register()
{
    walk_l_=0;
}

Request_register::Request_register(Complex_walker*w_l)
{
    walk_l_=w_l;    
}

void
Request_register::pre_move_processing()
{    
    do_pre_move_process();
    accepted_req_arr_.set_size(0);
}
void
Request_register::post_move_processing()
{    
    do_post_move_process();
}

Request_register::Request_register(Request_register const&)
{
    assert(false);
}

void
Request_register::announce_element(Staff_elem_info i)
{
    walk_l_->announce_element(i);
}

void
Request_register::typeset_element(Staff_elem*p)
{
    walk_l_->typeset_element(p);
}

Paperdef*
Request_register::paper()const
{
    return walk_l_->staff()->paper();
}
