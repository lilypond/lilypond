/*
  engraver.cc -- implement Engraver

  Sourcefile of GNU LilyPond musictypesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "music-list.hh"
#include "musical-request.hh"
#include "engraver.hh"
#include "engraver-group.hh"
#include "debug.hh"

void
Engraver::post_move_processing()
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

void
Engraver::removal_processing()
{
    if ( status < CREATION_INITED)
	do_creation_processing();
    
    do_removal_processing();
}

bool
Engraver::try_request (Request * r)
{
    if (status < MOVE_INITED)
	post_move_processing();

    return do_try_request (r);
}

void
Engraver::process_requests()
{
    if (status < PROCESSED_REQS)
	post_move_processing();
    else if (status >= PROCESSED_REQS)
	return; 
    
    status = PROCESSED_REQS;
    do_process_requests();
}

void
Engraver::pre_move_processing()
{
    do_pre_move_processing();
    status = CREATION_INITED;
}

void
Engraver::fill_staff_info (Staff_info&)
{
    
}

Scalar
Engraver::get_feature (String t)
{
    return daddy_grav_l_->get_feature (t);
}

bool
Engraver::do_try_request (Request*)
{
    return false;
}

Engraver::Engraver()
{
    status = VIRGIN;
    daddy_grav_l_ = 0;
}

void
Engraver::announce_element (Score_elem_info i)
{
    i.origin_grav_l_arr_.push (this);
    daddy_grav_l_->announce_element (i);
}

void
Engraver::typeset_element (Score_elem*p)
{
    daddy_grav_l_->typeset_element (p);
}

Paper_def*
Engraver::paper()const
{
    return daddy_grav_l_->paper();
}

bool
Engraver::contains_b (Engraver *grav_l)const
{
    return this == grav_l;
}

Staff_info
Engraver::get_staff_info() const
{
    return daddy_grav_l_->get_staff_info();
}

void
Engraver::print() const
{
#ifndef NPRINT
    DOUT << "\n" << name() << " {";
    do_print();
    DOUT << "}";
#endif
}


IMPLEMENT_IS_TYPE_B(Engraver);

void
Engraver::do_print() const
{
}

