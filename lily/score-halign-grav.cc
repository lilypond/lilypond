/*
  score-halign-reg.cc -- implement Score_horizontal_align_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "bar.hh"
#include "break-align-item.hh"
#include "score-halign-grav.hh"
#include "score-align-grav.hh"

Score_horizontal_align_engraver::Score_horizontal_align_engraver()
{
  halign_p_ =0;
}

void
Score_horizontal_align_engraver::do_pre_move_processing()
{
  if (halign_p_) 
    {
	typeset_element (halign_p_);
	halign_p_ =0;
    }
	
}

void
Score_horizontal_align_engraver::acknowledge_element (Score_elem_info i)
{
  Engraver* reg = i.origin_grav_l_arr_[0];
  if (reg->is_type_b (
	Score_align_engraver::static_name()))
  {
	Score_align_engraver * align_grav_l = (Score_align_engraver*) reg;
	if (!halign_p_) 
	  {
	    halign_p_ = new Break_align_item;
	    halign_p_->breakable_b_ = true;
	    announce_element (Score_elem_info (halign_p_,0));
	  }
	Item * it = i.elem_l_->item();
	if (align_grav_l->type_ch_C_ == Bar::static_name())
	    halign_p_->center_l_ = it;
	
	halign_p_->add (it, align_grav_l->priority_i_);
    }
}

IMPLEMENT_IS_TYPE_B1(Score_horizontal_align_engraver,Engraver);
ADD_THIS_ENGRAVER(Score_horizontal_align_engraver);
