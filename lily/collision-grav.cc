/*
  collision-reg.cc -- implement Collision_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "note-column.hh"
#include "collision-grav.hh"
#include "collision.hh"

void
Collision_engraver::acknowledge_element (Score_elem_info i)
{
    if (!(i.elem_l_->name() == Note_column::static_name ()))
	return;

    if (!col_p_) {
	col_p_ = new Collision;
	announce_element (Score_elem_info (col_p_,0));
    }
    col_p_->add ((Note_column*)i.elem_l_->item());
}

void
Collision_engraver::do_pre_move_processing()
{
    if (col_p_) {
	typeset_element (col_p_);
	col_p_ =0;
    }
}
Collision_engraver::Collision_engraver()
{
    col_p_ =0;
}


IMPLEMENT_IS_TYPE_B1(Collision_engraver,Engraver);
ADD_THIS_ENGRAVER(Collision_engraver);
