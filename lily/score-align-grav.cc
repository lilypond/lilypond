/*
  score-align-reg.cc -- implement Type_align_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "horizontal-group-item.hh"
#include "score-align-grav.hh"
#include "item.hh"

Type_align_engraver::Type_align_engraver()
{
  type_ch_C_ = 0;
  priority_i_ =0;
  align_p_=0;
}

void
Type_align_engraver::do_pre_move_processing()
{
  if (align_p_) 
    {
      typeset_element (align_p_);
      align_p_ =0;
    }
}

void
Type_align_engraver::acknowledge_element (Score_elem_info inf)
{
  if (inf.elem_l_->is_type_b (type_ch_C_)) 
    {
  
      if (!align_p_) 
	{
	  align_p_ = new Horizontal_group_item;
	  align_p_->breakable_b_ = true;
	  announce_element (Score_elem_info (align_p_,0));
	}
      Score_elem * unbound_elem = inf.elem_l_;
      while (unbound_elem->axis_group_l_a_[X_AXIS])
	unbound_elem = unbound_elem->axis_group_l_a_[X_AXIS];
      align_p_->add_element (unbound_elem);
    }
  
}

IMPLEMENT_IS_TYPE_B1(Type_align_engraver,Engraver);
