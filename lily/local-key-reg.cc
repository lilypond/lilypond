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

Local_key_register::Local_key_register()
{
    key_item_p_ = 0;
    key_C_ = 0;
}

void
Local_key_register::pre_move_processing()
{
    if (key_item_p_) {
	typeset_element(key_item_p_);
	key_item_p_ = 0;
    }
}

void
Local_key_register::acknowledge_element(Staff_elem_info info)
{    
    if (info.req_l_->note()) {
	Note_req * note_l_ = info.req_l_->note();

	if( note_l_->forceacc_b_ ||
	    local_key_.oct(note_l_->octave_i_).acc(note_l_->notename_i_)
	    != note_l_->accidental_i_) {
	    Item * support_l_ = info.elem_l_->item();
	

	    if (!key_item_p_) {
		key_item_p_ = new Local_key_item(*get_staff_info().c0_position_i_l_);
	    }
	    
	    key_item_p_->add(note_l_);
	    key_item_p_->add(support_l_);
	    local_key_.oct(note_l_->octave_i_)
		.set(note_l_->notename_i_, note_l_->accidental_i_);
	}
    } else if (info.req_l_->command()  && info.req_l_->command()->keychange()) { 
	Key_register * key_reg_l =
	    (Key_register*)info.origin_reg_l_arr_[0];
	key_C_ = &key_reg_l->key_;
	local_key_ = *key_C_;
    } else if (info.elem_l_->name() == Key_item::static_name()) {
	Key_register * key_reg_l =
	    (Key_register*)info.origin_reg_l_arr_[0];
	key_C_ = &key_reg_l->key_;
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
	    warning ("Help me! can't figure  current key");
    }
}

IMPLEMENT_STATIC_NAME(Local_key_register);
ADD_THIS_REGISTER(Local_key_register);
