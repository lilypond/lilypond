/*
  key-reg.cc -- implement Key_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>

  todo: key undo
  
  */
#include "key-reg.hh"
#include "key-item.hh"
#include "command-request.hh"
#include "staff-column.hh"
#include "local-key-reg.hh"
#include "musical-request.hh"
#include "local-key-item.hh"
#include "bar.hh"

Key_register::Key_register()
{
    post_move_processing();
}

bool
Key_register::try_request(Request * req_l)
{
    Command_req* creq_l= req_l->command();
    if (!creq_l|| !creq_l->keychange())
	return false;
     
    assert(!keyreq_l_);		// todo
    keyreq_l_ = creq_l->keychange();
    change_key_b_ = true;
    read_req(keyreq_l_);
    return true;
}

void
Key_register::acknowledge_element(Staff_elem_info info)
{
    Command_req * r_l = info.req_l_->command() ;
    if (r_l && r_l->clefchange()) {
	change_key_b_ = true;
    }
    
    if (info.elem_l_->name() == Bar::static_name()) 
	default_key_b_ = true;

}

void
Key_register::process_requests()
{
    int c0_i= *get_staff_info().c0_position_i_l_;

    if (key_.multi_octave_b_)
	assert(false); // TODO . 
    else 
	kit_p_ = new Key_item(c0_i);
    kit_p_->read(*this);
    announce_element(Staff_elem_info(kit_p_, keyreq_l_));
}

void
Key_register::pre_move_processing()
{ 

    if (! default_key_b_ && ! change_key_b_ ) {
	delete kit_p_ ;
	kit_p_ =0;
    }
    
    if (kit_p_) {
	if (change_key_b_) 
	    typeset_breakable_item(
		new Key_item(*kit_p_), kit_p_, new Key_item(*kit_p_));
	else 
	    typeset_breakable_item(0,0,kit_p_);
	kit_p_ = 0;
    }
}


    
void
Key_register::read_req(Key_change_req * r)
{
    key_.multi_octave_b_ = r->multi_octave_b_;
    accidental_idx_arr_.set_size(0);
    for (int i = 0; i < r->melodic_p_arr_.size(); i ++) {
	Melodic_req *  m_l =r->melodic_p_arr_[i];
	int n_i=m_l->notename_i_;
	int a_i = m_l->accidental_i_;
	int o_i = m_l->octave_i_;
	if (r->multi_octave_b_)
	    key_.set(o_i, n_i, a_i);
	else
	    key_.set(n_i, a_i);
	accidental_idx_arr_.push(n_i);
    }
}

void
Key_register::post_move_processing()
{
    keyreq_l_ = 0;
    default_key_b_ = false;
    kit_p_ = 0;
    change_key_b_ = false;
}
IMPLEMENT_STATIC_NAME(Key_register);
ADD_THIS_REGISTER(Key_register);
