/*
  collision-reg.cc -- implement Collision_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "note-column.hh"
#include "collision-reg.hh"
#include "collision.hh"

void
Collision_register::acknowledge_element(Score_elem_info i)
{
    if (!(i.elem_l_->name() == Note_column::static_name()))
	return;

    if (!col_p_)
	col_p_ = new Collision;
    
    col_p_->add((Note_column*)i.elem_l_->item());
}

void
Collision_register::do_pre_move_processing()
{
    if (col_p_) {
	typeset_element(col_p_);
	col_p_ =0;
    }
}
Collision_register::Collision_register()
{
    col_p_ =0;
}

IMPLEMENT_STATIC_NAME(Collision_register);
IMPLEMENT_IS_TYPE_B1(Collision_register,Request_register);
ADD_THIS_REGISTER(Collision_register);
