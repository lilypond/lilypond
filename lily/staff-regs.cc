/*
  staff-regs.cc -- implement Staff_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "staff-sym.hh"
#include "voice-group-regs.hh"
#include "voice-regs.hh"
#include "staff-regs.hh"
#include "command-request.hh"
#include "bar.hh"
#include "debug.hh"
#include "input-register.hh"

Staff_info
Staff_registers::get_staff_info() 
{
    Staff_info inf;
    inf = Request_register::get_staff_info();
    inf.staff_sym_l_=staff_sym_l_;
    inf.c0_position_i_l_ = &c0_position_i_;
    return inf;
}

Staff_registers::Staff_registers(Input_register const*ireg_C)
{
    staff_sym_l_ =0;
    c0_position_i_ = 0;
    base_position_i_ =0;
    add( ireg_C->get_nongroup_p_arr());
    ireg_C_ =ireg_C;
}

/** Magic function which takes a Voice_registers out of one of its own
  children, and puts it in another. This also handles the push and
  popgroup feature.  */
void
Staff_registers::change_group(Group_change_req * greq_l,
			       Voice_registers *voice_regs_l,
			       Voice_group_registers * old_group)
{

    Voice_registers *regs_p = (old_group)
	? (Voice_registers*) old_group->get_register_p(
	    voice_regs_l)
	: new Voice_registers(
	    greq_l->voice_l(), ireg_C_->get_ireg_l("Voice_group_registers")
	    ->get_ireg_l("Voice_registers"));

    String new_str = greq_l->newgroup_str_;
    String old_str;
    if (old_group)
	 old_str = old_group->group_id_str_;
    if ( new_str[0] == '+') {
	new_str = old_str + new_str;
    } else if (new_str[0] == '-') {
	int idx = old_str.index_last_i('+');
	if (idx >=0)
	    new_str = old_str.left_str ( idx );
    }
    Voice_group_registers * new_group_l = get_group(new_str);
    new_group_l->add(regs_p);
    regs_p->sync_features(); 
    mtor << "processed change_group " << get_staff_info().when()<<"\n";
    print();
}

Voice_group_registers *
Staff_registers::get_group(String id)
{
    for (int i=0; i < group_l_arr_.size(); i++) {
	if (group_l_arr_[i]->group_id_str_ == id)
	    return group_l_arr_[i];
    }
    Voice_group_registers *group_p = 
	new Voice_group_registers(id, ireg_C_->get_ireg_l("Voice_group_registers"));
    group_l_arr_.push(group_p);
    add(group_p);
    return group_p;
}


void
Staff_registers::terminate_register(Request_register * reg)
{
    for (int i=0; i < group_l_arr_.size(); i++) {
	if (group_l_arr_[i] == reg) {
	    group_l_arr_.del(i);
	    Register_group_register::terminate_register(reg);
	    return;
	}
    }
    assert(false);
}

bool
Staff_registers::try_request(Request * r)
{
    bool b = Register_group_register::try_request(r);
    if (!b) {
	Command_req * cr_l = r->command() ;
	
	if (cr_l && cr_l->groupchange()) {
	    change_group(cr_l->groupchange(), 0, 0);
	    b = true;
	} else 
	    b= false;
    }
    return b;
}

IMPLEMENT_STATIC_NAME(Staff_registers);

bool
Staff_registers::acceptable_request_b(Request*r)const
{
    return Register_group_register::acceptable_request_b(r) ||
	(r->command() && r->command()->groupchange());
}

void
Staff_registers::acknowledge_element(Staff_elem_info i)
{
    Register_group_register::acknowledge_element(i);
    if ( i.elem_l_->name() == Staff_symbol::static_name())
	staff_sym_l_ = (Staff_symbol*)i.elem_l_;
}
