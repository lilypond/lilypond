/*
  engraver.cc -- implement Request_engraver

  Sourcefile of GNU LilyPond musictypesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "music-list.hh"
#include "musical-request.hh"
#include "engraver.hh"
#include "engraver-group.hh"
#include "debug.hh"

void
Request_engraver::post_move_processing()
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
Request_engraver::try_request(Request * r)
{
    if (status < MOVE_INITED)
	post_move_processing();

    return do_try_request(r);
}

void
Request_engraver::process_requests()
{
    if (status < PROCESSED_REQS)
	post_move_processing();
    else if (status >= PROCESSED_REQS)
	return; 
    
    status = PROCESSED_REQS;
    do_process_requests();
}

void
Request_engraver::pre_move_processing()
{
    do_pre_move_processing();
    status = CREATION_INITED;
}

void
Request_engraver::fill_staff_info(Staff_info&)
{
    
}

Scalar
Request_engraver::get_feature(String t)
{
    return daddy_grav_l_->get_feature(t);
}

bool
Request_engraver::do_try_request(Request*)
{
    return false;
}

Request_engraver::Request_engraver()
{
    status = VIRGIN;
    daddy_grav_l_ = 0;
}

void
Request_engraver::announce_element(Score_elem_info i)
{
    i.origin_grav_l_arr_.push(this);
    daddy_grav_l_->announce_element(i);
}

void
Request_engraver::typeset_element(Score_elem*p)
{
    daddy_grav_l_->typeset_element(p);
}

Paper_def*
Request_engraver::paper()const
{
    return daddy_grav_l_->paper();
}

void
Request_engraver::typeset_breakable_item(Item * nobreak_p)
{
    daddy_grav_l_->typeset_breakable_item(nobreak_p);
}

bool
Request_engraver::contains_b(Request_engraver *grav_l)const
{
    return this == grav_l;
}

Staff_info
Request_engraver::get_staff_info() const
{
    return daddy_grav_l_->get_staff_info();
}

void
Request_engraver::print() const
{
#ifndef NPRINT
    mtor << "\n" << name() << " {";
    do_print();
    mtor << "}";
#endif
}

IMPLEMENT_STATIC_NAME(Request_engraver);
IMPLEMENT_IS_TYPE_B(Request_engraver);

void
Request_engraver::do_print()const
{
}


