/*
  collision-reg.cc -- implement Collision_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "note-column.hh"
#include "collision-grav.hh"
#include "collision.hh"

void
Collision_engraver::acknowledge_element (Score_elem_info i)
{
  if (i.elem_l_->is_type_b (Note_column::static_name ()))
    {
      Note_column * c = (Note_column*) i.elem_l_->item ();
      if (c->rest_b ())
	return ;
      if (!col_p_) 
	{
	  col_p_ = new Collision;
	  announce_element (Score_elem_info (col_p_,0));
	}
      col_p_->add (c);
    }
}
void
Collision_engraver::do_pre_move_processing()
{
  if (col_p_) 
    {
      typeset_element (col_p_);
      col_p_ =0;
    }
}
Collision_engraver::Collision_engraver()
{
  col_p_ =0;
}


IMPLEMENT_IS_TYPE_B1(Collision_engraver,Engraver);
ADD_THIS_TRANSLATOR(Collision_engraver);
