/*
  rest-collision-reg.cc -- implement Rest_collision_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "rest-collision.hh"
#include "rest-collision-reg.hh"
#include "collision.hh"
#include "rest-column.hh"
#include "note-column.hh"

void
Rest_collision_register::acknowledge_element(Score_elem_info i)
{
    char const * nC = i.elem_l_->name();
    if (nC == Collision::static_name()) {
	collision_l_arr_.push((Collision*)i.elem_l_->item());
    } 
    else if (nC == Rest_column ::static_name()) {
	if (!rest_collision_p_)
	    rest_collision_p_ = new Rest_collision;
	rest_collision_p_->add((Rest_column*)i.elem_l_->item());
    }
}

void
Rest_collision_register::pre_move_processing()
{
    if (rest_collision_p_) {
	typeset_element(rest_collision_p_);
	rest_collision_p_ = 0;
    }
}

Rest_collision_register::Rest_collision_register()
{
    rest_collision_p_ =0;
}

IMPLEMENT_STATIC_NAME(Rest_collision_register);
ADD_THIS_REGISTER(Rest_collision_register);
