/*
  local-key-reg.cc -- implement Local_key_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musical-request.hh"
#include "command-request.hh"
#include "local-key-reg.hh"
#include "local-key-item.hh"
#include "complex-walker.hh"
#include "key-reg.hh"
#include "debug.hh"
#include "key-item.hh"
#include "tie.hh"
#include "notehead.hh"

Local_key_register::Local_key_register()
{
    key_C_ = 0;
}

void
Local_key_register::pre_move_processing()
{
    if (mel_l_arr_.size()) {
	Local_key_item *key_item_p = 0;
	for (int i=0; i  < mel_l_arr_.size(); i++) {
	    Item * support_l = support_l_arr_[i];

	    Note_req * note_l = mel_l_arr_[i];
	    if (tied_l_arr_.find_l(support_l) && 
		!forced_l_arr_.find_l(support_l))
		continue;
	    
	    if (!key_item_p)
		key_item_p = new Local_key_item(*get_staff_info().c0_position_i_l_);
	    key_item_p->add(note_l);
	    key_item_p->add(support_l);
	    local_key_.oct(note_l->octave_i_)
		.set(note_l->notename_i_, note_l->accidental_i_);
	}
	if (key_item_p)
	    typeset_element(key_item_p);
    }
    mel_l_arr_.set_size(0);
    tied_l_arr_.set_size(0);
    support_l_arr_.set_size(0);
    forced_l_arr_.set_size(0);	
}

void
Local_key_register::acknowledge_element(Score_elem_info info)
{    
    Score_elem * elem_l = info.elem_l_;
    if (info.req_l_->note()) {
	Note_req * note_l = info.req_l_->note();
	Item * item_l = info.elem_l_->item();

	if( note_l->forceacc_b_ ||
	    local_key_.oct(note_l->octave_i_).acc(note_l->notename_i_)
	    != note_l->accidental_i_) {
	    mel_l_arr_.push(note_l );
	    support_l_arr_.push(item_l);
	    if (note_l->forceacc_b_)
		forced_l_arr_.push(item_l);
	}
    } else if (info.req_l_->command()
	       && info.req_l_->command()->keychange()) { 
	Key_register * key_reg_l =
	    (Key_register*)info.origin_reg_l_arr_[0];
	key_C_ = &key_reg_l->key_;
	local_key_ = *key_C_;
    } else if (elem_l->name() == Key_item::static_name()) {
	Key_register * key_reg_l =
	    (Key_register*)info.origin_reg_l_arr_[0];
	key_C_ = &key_reg_l->key_;
    } else if (elem_l->name() == Tie::static_name()) {
	Tie * tie_l = (Tie*)elem_l->spanner();
	if (tie_l->same_pitch_b_)
	    tied_l_arr_.push(tie_l-> right_head_l_ );
    }
}

void
Local_key_register::process_requests()
{
    Time_description const * time_C_ = get_staff_info().time_C_;
    if (! time_C_->whole_in_measure_){
	if (key_C_)
	    local_key_= *key_C_;
	else if( time_C_->when_ >Moment(0))
	    warning ("Help me! can't figure out current key");
    }
}

IMPLEMENT_STATIC_NAME(Local_key_register);
ADD_THIS_REGISTER(Local_key_register);
