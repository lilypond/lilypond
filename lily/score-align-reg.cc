/*
  score-align-reg.cc -- implement Score_align_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "elem-group-item.hh"
#include "score-align-reg.hh"
#include "item.hh"

Score_align_register::Score_align_register(const char *nm)
{
    type_ch_C_ = nm;
    priority_i_ =0;
    align_p_=0;
}

void
Score_align_register::do_pre_move_processing()
{
    if (align_p_) {
	typeset_breakable_item( align_p_);
	align_p_ =0;
    }
}

void
Score_align_register::acknowledge_element(Score_elem_info inf)
{
    if (inf.elem_l_->name() == type_ch_C_ ) {
	if (! align_p_ ) {
	    align_p_ = new Horizontal_group_item;
	    announce_element(Score_elem_info(align_p_,0));
	}
	
	align_p_->add_element(inf.elem_l_);
    }
}

IMPLEMENT_STATIC_NAME(Score_align_register)
IMPLEMENT_IS_TYPE_B1(Score_align_register,Request_register);
