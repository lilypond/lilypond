/*
  score-align-reg.cc -- implement Score_align_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "elem-group-item.hh"
#include "score-align-grav.hh"
#include "item.hh"

Score_align_engraver::Score_align_engraver()
{
    type_ch_C_ = 0;
    priority_i_ =0;
    align_p_=0;
}

void
Score_align_engraver::do_pre_move_processing()
{
    if (align_p_) {
	typeset_breakable_item( align_p_);
	align_p_ =0;
    }
}

void
Score_align_engraver::acknowledge_element(Score_elem_info inf)
{
    if (inf.elem_l_->name() == type_ch_C_ ) {
	
	if (! align_p_ ) {
	    align_p_ = new Horizontal_group_item;
	    announce_element(Score_elem_info(align_p_,0));
	}
	
	align_p_->add_element(inf.elem_l_);
    }
}

IMPLEMENT_STATIC_NAME(Score_align_engraver)
IMPLEMENT_IS_TYPE_B1(Score_align_engraver,Request_engraver);
