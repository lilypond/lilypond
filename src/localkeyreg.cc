/*
  localkeyreg.cc -- implement Local_key_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "musicalrequest.hh"
#include "commandrequest.hh"
#include "localkeyreg.hh"
#include "localkeyitem.hh"
#include "complexwalker.hh"
#include "keyreg.hh"
#include "debug.hh"

Local_key_register::Local_key_register(Complex_walker*w)
    : Request_register(w)
{
    key_item_p_ = 0;
    key_c_l_ = 0;
}

void
Local_key_register::do_pre_move_process()
{
    if (key_item_p_) {
	walk_l_->typeset_element(key_item_p_);
	key_item_p_ = 0;
    }
}
void
Local_key_register::acknowledge_element(Staff_elem_info info)
{    
    if (info.req_l_->melodic()) {
	Melodic_req * melodic_l_ = info.req_l_->melodic();

	if( melodic_l_->forceacc_b_ ||
	    local_key_.oct(melodic_l_->octave_i_).acc(melodic_l_->notename_i_)
	    != melodic_l_->accidental_i_) {
	    Item * support_l_ = info.elem_p_->item();
	

	    if (!key_item_p_) {
		key_item_p_ = new Local_key_item(walk_l_->c0_position_i());
	    }
	    
	    key_item_p_->add(melodic_l_);
	    key_item_p_->add(support_l_);
	    local_key_.oct(melodic_l_->octave_i_)
		.set(melodic_l_->notename_i_, melodic_l_->accidental_i_);
	}
    } else if (info.req_l_->command() &&
	       info.req_l_->command()->keychange()) {
	Key_register * key_reg_l = (Key_register*)info.origin_reg_l_;
	key_c_l_ = &key_reg_l->key_;
	local_key_.reset(*key_c_l_);
    }	
}

void
Local_key_register::process_request()
{
    if (!walk_l_->time_.whole_in_measure_) {
	if (key_c_l_)  
	    local_key_.reset(*key_c_l_);
	else if( walk_l_->when() >0)
	    warning ("Help me! can't figure  current key", 0);
    }
}
