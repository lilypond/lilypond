/*
  rest-collision-reg.cc -- implement Rest_collision_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "rest-collision.hh"
#include "rest-collision-grav.hh"
#include "collision.hh"
#include "note-column.hh"


IMPLEMENT_IS_TYPE_B1(Rest_collision_engraver, Engraver);
ADD_THIS_TRANSLATOR(Rest_collision_engraver);

Rest_collision_engraver::Rest_collision_engraver()
{
  rest_collision_p_ =0;
}

void
Rest_collision_engraver::acknowledge_element (Score_elem_info i)
{
  if (i.elem_l_->is_type_b (Note_column::static_name())) 
    {
      // what should i do, what should _engraver do?
      if (!rest_collision_p_) 
	{
	  rest_collision_p_ = new Rest_collision;
	  announce_element (Score_elem_info (rest_collision_p_, 0));
	}
      rest_collision_p_->add ((Note_column*)i.elem_l_->item());
    }
}

void
Rest_collision_engraver::do_pre_move_processing()
{
  if (rest_collision_p_) 
    {
      typeset_element (rest_collision_p_);
      rest_collision_p_ = 0;
    }
}
