/*
  clef.cc -- implement  Clef_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>,
  Mats Bengtsson <matsb@s3.kth.se>
*/

#include "clefreg.hh"
#include "clefitem.hh"
#include "debug.hh"
#include "commandrequest.hh"
#include "timedescription.hh"
#include "complexwalker.hh"
#include "staffcolumn.hh"

Clef_register::Clef_register(Complex_walker*w)
    : Request_register(w)
{
    clef_p_ = 0;
    
    set_type("violin");
}
bool
Clef_register::set_type(String s)
{
    clef_type_str_  = s;
    if (clef_type_str_ == "violin") {
	c0_position_i_= -2;
    } else if (clef_type_str_ == "alto") {
	c0_position_i_= 4;
    } else if (clef_type_str_ == "tenor") {
	c0_position_i_= 6;
    } else if (clef_type_str_ == "bass") {
	c0_position_i_= 10;
    }else 
	return false;
    walk_l_->set_c0_position(c0_position_i_);
    return true;
}

void 
Clef_register::read_req(Clef_change_req*c_l)
{
    if (!set_type(c_l->clef_str_))
	error("unknown clef type ", c_l->defined_ch_c_l_);
}

bool
Clef_register::try_request(Request * r_l)
{
    
    Command_req* creq_l= r_l->command();
    if (!creq_l || !creq_l->clefchange())
	return false;

    accepted_req_arr_.push(creq_l);
    // do it now! Others have to read c0_pos.
    read_req(creq_l->clefchange()); 
    return true;
}

void
Clef_register::process_request()
{
    const Time_description *time_l = &walk_l_->time_;
    if (!accepted_req_arr_.size() &&
	(!time_l->whole_in_measure_|| !time_l->when_)) {

	clef_p_ = new Clef_item;
	clef_p_->change = false;
    } else if (accepted_req_arr_.size()) {
	clef_p_ = new Clef_item;
	clef_p_->change = true;
    }
    if (clef_p_) {
	clef_p_->read(*this);
//	announce_element(Staff_elem_info(clef_p_,
//					 accepted_req_arr_[0], this));
    }
}

void
Clef_register::do_pre_move_process()
{
    if (!clef_p_)
	return;
    if (clef_p_->change) {
	typeset_breakable_item(new Clef_item(*clef_p_),
			       clef_p_, new Clef_item(*clef_p_) );
    } else {
	typeset_breakable_item(0, 0, clef_p_);
    }
    clef_p_ = 0;
}
    
