/*
  voicegroup.cc -- implement Voice_group_registers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "music-list.hh"
#include "proto.hh"
#include "plist.hh"
#include "musical-request.hh"
#include "voice-regs.hh"
#include "voice-group-regs.hh"
#include "register.hh"
#include "command-request.hh"
#include "debug.hh"
#include "input-register.hh"
#include "time-description.hh"


Voice_group_registers::Voice_group_registers()
{
    dir_i_ =0;
    termination_mom_ = INFTY; 
}

bool
Voice_group_registers::do_try_request(Request*r_l)
{
    Command_req* c_l = r_l->command();
    if (c_l&& c_l->groupfeature()) {
	Feature f;
	f.type_ = c_l->groupfeature()->type_str_;
	f.value_ = c_l->groupfeature()->value_str_;
	set_feature(f);
	return true;
    }
    return Register_group_register::do_try_request(r_l);
}


IMPLEMENT_STATIC_NAME(Voice_group_registers);
IMPLEMENT_IS_TYPE_B1(Voice_group_registers,Register_group_register);

void
Voice_group_registers::do_print() const
{
#ifndef NPRINT
    Register_group_register::do_print();
#endif
}

void
Voice_group_registers::do_post_move_processing()
{
    if ( get_staff_info().time_C_ ->when_ > termination_mom_ ){
	mtor << "Terminating voice_group\n";
	daddy_reg_l_->terminate_register(this);
	return ;
    }
    Register_group_register::do_post_move_processing();
}



Scalar
Voice_group_registers::get_feature(String f)
{
    if (f == "vdir")
	return dir_i_;
    Register_group_register::get_feature(f);
}

ADD_THIS_REGISTER(Voice_group_registers);
