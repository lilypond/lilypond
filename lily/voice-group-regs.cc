/*
  voicegroup.cc -- implement Voice_group_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "musicalrequest.hh"
#include "voice-regs.hh"
#include "voice-group-regs.hh"
#include "register.hh"
#include "text-reg.hh"
#include "stem-beam-reg.hh"
#include "script-reg.hh"
#include "complex-walker.hh"
#include "commandrequest.hh"
#include "debug.hh"

static int temp_id_count;

Voice_group_registers::Voice_group_registers(String id)
{
    add(new Text_register);
    add(new Stem_beam_register);
    add(new Script_register);
    
    if (id=="")			// UGH
	id = __FUNCTION__ + String(temp_id_count++);
    group_id_str_ = id;
}

bool
Voice_group_registers::try_request(Request*r_l)
{
    for (int i=0; i < voice_regs_l_.size(); i++) {	
	if (voice_regs_l_[i]->voice_l_ == r_l->voice_l())
	    goto gotcha;
    }
    return false;
gotcha:
    if (r_l->groupfeature()) {
	set_feature(Features::dir(r_l->groupfeature()->stemdir_i_));
	return true;
    }
    return Register_group_register::try_request(r_l);
}


bool
Voice_group_registers::static_acceptable_request_b(Request*r)
{
    return (r->stem() || r->beam() || r->text() || r->script() ||
	    r->groupfeature());
}

void
Voice_group_registers::terminate_register(Request_register*r_l)
{
    if (r_l->name() == Voice_registers::static_name()) {
	for (int i=0; i <voice_regs_l_.size(); i++) {
	    if (r_l == voice_regs_l_[i])
		voice_regs_l_.del(i);
	    Register_group_register::terminate_register(r_l);
	    return;
	}
    }
    assert(false);
}
void
Voice_group_registers::do_print() const
{
#ifndef NPRINT
    mtor << "ID: " << group_id_str_<<"\n";
    Register_group_register::do_print();
#endif
}
void
Voice_group_registers::add(Request_register*r_l)
{
    Register_group_register::add(r_l);
    if (r_l->name() == Voice_registers::static_name())
	voice_regs_l_.push( (Voice_registers*)r_l );
}
