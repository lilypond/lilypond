/*
  keyreg.cc -- implement Key_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>

  todo: key undo
  
  */

#include "keyreg.hh"
#include "keyitem.hh"
#include "complexwalker.hh"
#include "commandrequest.hh"
#include "staffcolumn.hh"
#include "localkeyreg.hh"
#include "musicalrequest.hh"

Key_register::Key_register(Complex_walker*w)
    : Request_register(w)
{
    do_post_move_process();
}


bool
Key_register::try_request(Request * req_l)
{
    if (!req_l->command()->keychange())
	return false;
    assert(!keyreq_l_);		// todo
    keyreq_l_ = req_l->command()->keychange();
    read_req(keyreq_l_);
    return true;
}

void
Key_register::acknowledge_element(Staff_elem_info info)
{
    Command_req * r_l = info.req_l_->command() ;
    if (r_l && r_l->clefchange() && !kit_p_) {
	 kit_p_ = new Key_item(walk_l_->c0_position_i());
	 kit_p_->read(*this);
	 announce_element(Staff_elem_info(kit_p_, keyreq_l_, this));
    }
}

void
Key_register::process_request()
{
    Time_description *time_l = &walk_l_->time_;

    if (!keyreq_l_ &&
	(!time_l->whole_in_measure_|| !time_l->when_)) {
	default_key_b_ = true;
    }

     if ( default_key_b_ || keyreq_l_) {
	 kit_p_ = new Key_item(walk_l_->c0_position_i());
	 kit_p_->read(*this);
	 announce_element(Staff_elem_info(kit_p_, keyreq_l_, this));
     }
}

void
Key_register::do_pre_move_process()
{
    if (kit_p_) {
	if (default_key_b_) 
	    typeset_breakable_item(0,0,kit_p_);
	else 
	    typeset_breakable_item(
		new Key_item(*kit_p_), kit_p_, new Key_item(*kit_p_));
	kit_p_ = 0;
    }
}


    
void
Key_register::read_req(Key_change_req * r)
{
    accidental_idx_arr_.set_size(0);
    for (int i = 0; i < r->melodic_p_arr_.size(); i ++) {
	int n_i=r->melodic_p_arr_[i]->notename_i_;
	key_.set(n_i, r->melodic_p_arr_[i]->accidental_i_);
	accidental_idx_arr_.push(n_i);
    }
}

void
Key_register::do_post_move_process()
{
    keyreq_l_ = 0;
    default_key_b_ = false;
    kit_p_ = 0;
}
