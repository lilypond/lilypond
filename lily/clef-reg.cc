/*
  clef.cc -- implement  Clef_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>,
  Mats Bengtsson <matsb@s3.kth.se>
*/

#include "clef-reg.hh"
#include "clef-item.hh"
#include "debug.hh"
#include "commandrequest.hh"
#include "time-description.hh"
#include "staff-column.hh"

Clef_register::Clef_register()
{
    clef_p_ = 0;
    
    /* ugly hack to prevent segfault (daddy_reg_l_ == 0 at construction) */
    clef_type_str_ = "";	
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
    *get_staff_info().c0_position_i_ = c0_position_i_;
    
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

    clef_req_l_ = creq_l->clefchange();
    
    // do it now! Others have to read c0_pos.
    read_req(creq_l->clefchange()); 
    return true;
}

void
Clef_register::process_requests()
{
    const Time_description *time_l = get_staff_info().time_c_l_;
    if (!clef_req_l_ && (!time_l->whole_in_measure_|| !time_l->when_)) {
	clef_p_ = new Clef_item;
	clef_p_->change = false;
    } else if (clef_req_l_) {
	clef_p_ = new Clef_item;
	clef_p_->change = true;
    }
    if (clef_p_) {
	clef_p_->read(*this);
	announce_element(Staff_elem_info(clef_p_,
					 clef_req_l_));
    }
}

void
Clef_register::pre_move_processing()
{
    if (!clef_p_)
	return;
    if (clef_p_->change) {
	Clef_item* post_p = new Clef_item(*clef_p_);
	post_p->change = false;
	typeset_breakable_item(new Clef_item(*clef_p_),
			       clef_p_,  post_p);
    } else {
	typeset_breakable_item(0, 0, clef_p_);
    }
    clef_p_ = 0;
}
    
void
Clef_register::post_move_processing()
{
    clef_req_l_ = 0;
    /* not in ctor, since the reg might not be linked in.*/
    if (clef_type_str_ == "") {	
	set_type("violin");
    }
}
