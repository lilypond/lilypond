/*
  voice-regs.cc -- implement Voice_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "proto.hh"
#include "plist.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "voice-regs.hh"
#include "register.hh"
#include "staff-regs.hh"	// needed because somebody has to delete us.
#include "debug.hh"
#include "input-register.hh"
#include "voice-group-regs.hh"

Voice_registers::Voice_registers(Voice *v_p, Input_register const*ireg_C)
{
    terminate_b_ = false;
    ireg_C_ = ireg_C;
    voice_l_ = v_p;
    add(ireg_C->get_nongroup_p_arr());
}

void
Voice_registers::acknowledge_element(Score_elem_info i)
{
    if (i.voice_l_ != voice_l_)
	return;
    Register_group_register::acknowledge_element(i);
}

bool
Voice_registers::try_request(Request*r_l)
{
    if (r_l->voice_l() !=voice_l_)
	return false;
    
    Command_req *c=r_l->command();
    if (c&&c->terminate()) {
	terminate_b_ = true;
	return true;		// scary. We're deleted now.. 
    } else if (c&&c->groupchange()) {
	/* this is scary as well. The groupchange has to be handled by
	 the Staff_registers, which are two levels up in the hierarchy
	 */
	  
	assert(daddy_reg_l_->name() == Voice_group_registers::static_name());
	((Staff_registers*)daddy_reg_l_->daddy_reg_l_)->	// scary.
	    change_group(c->groupchange(), this,
			 (Voice_group_registers*)daddy_reg_l_);	// ugh!
	return true;
    }
    
    return Register_group_register::try_request(r_l);
}

bool
Voice_registers::acceptable_request_b(Request*r)
{
    Command_req *  c_l = r->command();
    return   (c_l&&(c_l->terminate()||c_l->groupchange()))
	|| Register_group_register::acceptable_request_b(r);
}
IMPLEMENT_STATIC_NAME(Voice_registers);

void
Voice_registers::do_print() const
{
#ifndef NPRINT
    mtor << "Voice= " << voice_l_<<'\n';
    Register_group_register::do_print();
#endif
}

void
Voice_registers::pre_move_processing()
{
    if (terminate_b_)
	daddy_reg_l_->terminate_register(this);
    else 
	Register_group_register::pre_move_processing();
}
