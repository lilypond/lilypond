/*
  register.cc -- implement Request_register

  Sourcefile of LilyPond musictypesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "voice.hh"
#include "musicalrequest.hh"
#include "register.hh"
#include "notehead.hh"
#include "complexwalker.hh"
#include "localkeyitem.hh"
#include "complexstaff.hh"


bool
Request_register::try_request(Request*)
{
    return false;
}

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

void
Request_register::typeset_breakable_item(Item * pre_p , Item * nobreak_p,
					 Item * post_p)
{
    walk_l_->typeset_breakable_item(pre_p,  nobreak_p,post_p);
}

bool
Request_register::acceptable_request_b(Request*)const
{
    return false;
}
