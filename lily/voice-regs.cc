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
#include "slur-reg.hh"
#include "headreg.hh"
#include "walk-regs.hh"
#include "debug.hh"

Voice_registers::Voice_registers(Voice *v_p)
{
    voice_l_ = v_p;
    add(new Notehead_register);
    add(new Slur_register);
}

void
Voice_registers::acknowledge_element(Staff_elem_info i)
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
	daddy_reg_l_->terminate_register(this);
	return true;		// scary. We're deleted now.. 
    } else if (c&&c->groupchange()) {

	((Walker_registers*)daddy_reg_l_->daddy_reg_l_)->	// scary.
	    change_group(c->groupchange(), this,
			 (Voice_group_registers*)daddy_reg_l_);	// UGR!
	return true;
    }
    
    return Register_group_register::try_request(r_l);
}

bool
Voice_registers::acceptable_request_b(Request*r)
{
    Command_req *  c_l = r->command();
    return  r->groupchange() || (c_l&&c_l->terminate())
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
