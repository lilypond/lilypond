/*
  rest-collision-reg.cc -- implement Rest_collision_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "rest-collision.hh"
#include "rest-collision-grav.hh"
#include "collision.hh"
#include "rest-column.hh"
#include "note-column.hh"

IMPLEMENT_STATIC_NAME(Rest_collision_engraver);
IMPLEMENT_IS_TYPE_B1(Rest_collision_engraver, Request_engraver);
ADD_THIS_ENGRAVER(Rest_collision_engraver);

Rest_collision_engraver::Rest_collision_engraver()
{
    rest_collision_p_ =0;
}

void
Rest_collision_engraver::acknowledge_element(Score_elem_info i)
{
    char const * nC = i.elem_l_->name();
    if (nC == Collision::static_name()) {
	collision_l_arr_.push((Collision*)i.elem_l_->item());
    } 
    else if (nC == Note_column::static_name()) {
	// what should i do, what should _engraver do?
	if (!rest_collision_p_)
	    rest_collision_p_ = new Rest_collision;
	rest_collision_p_->add((Note_column*)i.elem_l_->item());
    }
    else if (nC == Rest_column::static_name()) {
	if (!rest_collision_p_)
	    rest_collision_p_ = new Rest_collision;
	rest_collision_p_->add((Rest_column*)i.elem_l_->item());
    }
}

void
Rest_collision_engraver::do_pre_move_processing()
{
    if (rest_collision_p_) {
	typeset_element(rest_collision_p_);
	rest_collision_p_ = 0;
    }
}

void
Rest_collision_engraver::do_print() const
{
#ifndef NPRINT
    mtor << "collisions: " << collision_l_arr_.size();
    if ( rest_collision_p_ )
	rest_collision_p_->print();
#endif
}
