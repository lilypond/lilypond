/*
  score-halign-reg.cc -- implement Score_horizontal_align_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "break-align-item.hh"
#include "score-halign-reg.hh"
#include "score-align-reg.hh"

Score_horizontal_align_register::Score_horizontal_align_register()
{
    halign_p_ =0;
}

void
Score_horizontal_align_register::do_pre_move_processing()
{
    if (halign_p_) {
	typeset_breakable_item(halign_p_);
	halign_p_ =0;
    }
	
}

void
Score_horizontal_align_register::acknowledge_element(Score_elem_info i)
{
    Request_register* reg = i.origin_reg_l_arr_[0];
    if (reg->is_type_b( 
	Score_align_register::static_name()) )
    {
	if (!halign_p_) {
	    halign_p_ = new Break_align_item;
	    announce_element(Score_elem_info(halign_p_,0));
	}
	halign_p_->add(i.elem_l_->item(), 
		       ((Score_align_register*)reg)->priority_i_  );
    }
}
IMPLEMENT_STATIC_NAME(Score_horizontal_align_register);
IMPLEMENT_IS_TYPE_B1(Score_horizontal_align_register,Request_register);
ADD_THIS_REGISTER(Score_horizontal_align_register);
