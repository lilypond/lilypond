/*
  voicegroup.cc -- implement Voice_group_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "plist.hh"
#include "musicalrequest.hh"
#include "voiceregs.hh"
#include "voicegroupregs.hh"
#include "register.hh"
#include "textreg.hh"
#include "stembeamreg.hh"
#include "scriptreg.hh"
#include "complexwalker.hh"
#include "commandrequest.hh"

static int temp_id_count;

Voice_group_registers::Voice_group_registers(Complex_walker*w_l, String id)
{
    walk_l_ = w_l;
    add(new Text_register(w_l));
    add(new Stem_beam_register(w_l));
    add(new Script_register(w_l));
    
    if (id=="")			// UGH
	id = __FUNCTION__ + String(temp_id_count++);
    group_id_str_ = id;
}

bool
Voice_group_registers::try_request(Request*r_l)
{
    if (r_l->groupfeature()) {
	set_dir(r_l->groupfeature()->stemdir_i_);
	return true;
    }
    return Register_group::try_request(r_l);
}
    
void
Voice_group_registers::acknowledge_element(Staff_elem_info i)
{
    if (i.group_regs_l_!= this)
	return;
    Register_group::acknowledge_element(i);
}
#if 1
void
Voice_group_registers::set_dir(int i)
{
    Register_group::set_dir(i);

    // ughh
    Array<Voice_registers*> vr_arr (walk_l_->get_voice_regs(this));
    for (int j=0; j<vr_arr.size(); j++) {
	if (vr_arr[j])
	    vr_arr[j]->set_dir(i);
    }
}
#endif
bool
Voice_group_registers::acceptable_request_b(Request*r)
{
    return (r->stem() || r->beam() || r->text() || r->script() ||
	    r->groupfeature());
}
