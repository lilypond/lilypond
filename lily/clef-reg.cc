/*
  clef.cc -- implement  Clef_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>,
  Mats Bengtsson <matsb@s3.kth.se>
*/

#include "bar.hh"
#include "clef-reg.hh"
#include "clef-item.hh"
#include "debug.hh"
#include "command-request.hh"
#include "time-description.hh"

Clef_register::Clef_register()
{
    clef_p_ = 0;
    clef_req_l_ =0;
    
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
    } else 
	return false;
    
    return true;
}

void
Clef_register::fill_staff_info(Staff_info &i)
{
    i.c0_position_i_l_ = &c0_position_i_;
}

void 
Clef_register::read_req(Clef_change_req*c_l)
{
    if (!set_type(c_l->clef_str_))
	c_l->error("unknown clef type ");
}
void
Clef_register::acknowledge_element(Score_elem_info info)
{
    if (info.elem_l_->name() == Bar::static_name() ) {
	if (!clef_p_){
	    create_clef();
	    clef_p_->default_b_ = true;
	}
    }
}

void
Clef_register::do_creation_processing()
{
    create_clef();
    clef_p_->default_b_ = false;
}

bool
Clef_register::do_try_request(Request * r_l)
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
Clef_register::create_clef()
{
    if (!clef_p_) {
	clef_p_ = new Clef_item;
        announce_element(Score_elem_info(clef_p_,
				     clef_req_l_));
    
	clef_p_->read(*this);
    }

}
void
Clef_register::do_process_requests()
{
    if (clef_req_l_) {
	create_clef();
	clef_p_->default_b_ = false;
    }
}

void
Clef_register::do_pre_move_processing()
{
    if (!clef_p_)
	return;
    typeset_breakable_item(clef_p_);
    clef_p_ = 0;
}
    
void
Clef_register::do_post_move_processing()
{
    clef_req_l_ = 0;
}

void
Clef_register::do_removal_processing()
{
    delete clef_p_;
    clef_p_ =0;
}

IMPLEMENT_STATIC_NAME(Clef_register);
IMPLEMENT_IS_TYPE_B1(Clef_register,Request_register);
ADD_THIS_REGISTER(Clef_register);
