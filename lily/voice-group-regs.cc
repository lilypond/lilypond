/*
  voicegroup.cc -- implement Voice_group_registers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "voice.hh"
#include "proto.hh"
#include "plist.hh"
#include "musical-request.hh"
#include "voice-regs.hh"
#include "voice-group-regs.hh"
#include "register.hh"
#include "complex-walker.hh"
#include "command-request.hh"
#include "debug.hh"
#include "input-register.hh"

static int temp_id_count;

Voice_group_registers::Voice_group_registers(String id,
					     Input_register const *ireg_C)
{
    dir_i_ =0;
    ireg_C_ =ireg_C;
    Register_group_register::add(ireg_C->get_nongroup_p_arr());
    if (id=="")			// ugh
	id = __FUNCTION__ + String(temp_id_count++);
    group_id_str_ = id;
    termination_mom_ = 0; 
}

bool
Voice_group_registers::try_request(Request*r_l)
{
    for (int i=0; i < voice_reg_l_arr_.size(); i++) {	
	if (voice_reg_l_arr_[i]->voice_l_ == r_l->voice_l())
	    goto gotcha;	// yeah, yeah, I know
    }
    return false;
gotcha:
    Command_req* c_l = r_l->command();
    if (c_l&& c_l->groupfeature()) {
	Feature f;
	f.type_ = c_l->groupfeature()->type_str_;
	f.value_ = c_l->groupfeature()->value_str_;
	set_feature(f);
	return true;
    }
    return Register_group_register::try_request(r_l);
}


IMPLEMENT_STATIC_NAME(Voice_group_registers);

void
Voice_group_registers::do_print() const
{
#ifndef NPRINT
    mtor << "ID: " << group_id_str_<<"\n";
    mtor << "stopping at " << termination_mom_ << "\n";
    Register_group_register::do_print();
#endif
}
void
Voice_group_registers::add(Request_register*r_l)
{
    Register_group_register::add(r_l);
    if (r_l->name() == Voice_registers::static_name()) {
	Voice_registers * vregs_l = (Voice_registers*)r_l;
	voice_reg_l_arr_.push( vregs_l );
	Voice *v_l = vregs_l->voice_l_;
	termination_mom_ = termination_mom_ >? v_l -> last();
	mtor << "adding Voice_registers, now terminating at " << 
	    termination_mom_<< "\n";
    }
    OK();
}

void
Voice_group_registers::post_move_processing()
{
    if ( get_staff_info().time_C_ ->when_ > termination_mom_ ){
	mtor << "Terminating voice_group\n";
	daddy_reg_l_->terminate_register(this);
	return ;
    }
    Register_group_register::post_move_processing();
}

Request_register *
Voice_group_registers::get_register_p(Request_register *reg_l)
{
     if (reg_l->name() == Voice_registers::static_name()) {
	for (int i=0; i <voice_reg_l_arr_.size(); i++) {
	    if (reg_l == voice_reg_l_arr_[i])
		voice_reg_l_arr_.del(i);
	}
     }
     Request_register*reg_p =Register_group_register::get_register_p(reg_l);
     OK();

     return reg_p;
}

void
Voice_group_registers::OK() const
{
    for (int i=0; i < voice_reg_l_arr_.size(); i++) {
	iter(reg_list_.find(voice_reg_l_arr_[i]), j);
	assert(j.ok());
    }
}
	     
	     
void
Voice_group_registers::set_feature(Feature f)
{
    if (f.type_ == "vdir")
	dir_i_ = f.value_;
    Register_group_register::set_feature(f);
}

Scalar
Voice_group_registers::get_feature(String f)
{
    if (f == "vdir")
	return dir_i_;
    Register_group_register::get_feature(f);
}
