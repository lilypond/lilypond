/*
  score-halign-reg.cc -- implement Priority_horizontal_align_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "bar.hh"
#include "break-align-item.hh"
#include "priority-halign-engraver.hh"
#include "score-priority-engraver.hh"

Priority_horizontal_align_engraver::Priority_horizontal_align_engraver()
{
  halign_p_ =0;
}

void
Priority_horizontal_align_engraver::do_pre_move_processing()
{
  if (halign_p_) 
    {
      typeset_element (halign_p_);
      halign_p_ =0;
    }
}

void
Priority_horizontal_align_engraver::acknowledge_element (Score_element_info i)
{
  Engraver* reg = i.origin_grav_l_arr_[0];
  if (reg->is_type_b (Score_priority_engraver::static_name()))
    {
      if (!halign_p_) 
	{
	  halign_p_ = new Break_align_item;
	  halign_p_->breakable_b_ = true;
	  announce_element (Score_element_info (halign_p_,0));
	}
      Item * it = i.elem_l_->access_Item ();
      if (it->break_priority_i_ == 0)
	halign_p_->center_l_ = it;

      halign_p_->add_item (it, it->break_priority_i_);
    }
}

IMPLEMENT_IS_TYPE_B1(Priority_horizontal_align_engraver,Engraver);
ADD_THIS_TRANSLATOR(Priority_horizontal_align_engraver);
