/*
  walkregs.cc -- implement Walker_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "clef-reg.hh"
#include "local-key-reg.hh"
#include "key-reg.hh"
#include "meter-reg.hh"
#include "bar-reg.hh"
#include "bar.hh"
#include "walkregs.hh"
#include "staff-elem.hh"
#include "staff.hh"
#include "complex-walker.hh"
#include "staff-column.hh"
#include "voice-group-regs.hh"
#include "voice-regs.hh"
#include "commandrequest.hh"


Walker_registers::Walker_registers(Complex_walker *w)
{
    walk_l_ = w;
    add( new Bar_register);
    add( new Clef_register);
    add( new Key_register);
    add( new Meter_register);
    add( new Local_key_register);
}

void
Walker_registers::announce_element(Staff_elem_info info)
{
    if (info.elem_p_->name() == Bar::static_name()) {
	walk_l_->allow_break();
    }
    announce_info_arr_.push(info);
}

void
Walker_registers::acknowledge_element(Staff_elem_info )
{
    assert(false);
}

void
Walker_registers::do_announces()
{
    Request dummy_req;
    for (int i = 0; i < announce_info_arr_.size(); i++){
	Staff_elem_info info = announce_info_arr_[i];

	if (!info.req_l_)
	    info.req_l_ = &dummy_req;
	Register_group_register::acknowledge_element(info);
    }
    announce_info_arr_.set_size(0);
}

void
Walker_registers::typeset_element(Staff_elem *elem_p)
{
    typeset_musical_item(elem_p);
}

void
Walker_registers::typeset_musical_item(Staff_elem * elem_p)
{
    walk_l_->typeset_element(elem_p);
}

void
Walker_registers::typeset_breakable_item(Item * pre_p , Item * nobreak_p,
				       Item * post_p)
{
    if (pre_p)
	prebreak_item_p_arr_.push(pre_p);
    if (nobreak_p)
	nobreak_item_p_arr_.push(nobreak_p);
    if (post_p)
	postbreak_item_p_arr_.push(post_p);
}

void
Walker_registers::pre_move_processing()
{
    // this generates all items.
    Register_group_register::pre_move_processing();
    walk_l_->ptr()->typeset_breakable_items(prebreak_item_p_arr_,
					    nobreak_item_p_arr_,
					    postbreak_item_p_arr_);
}
void
Walker_registers::post_move_processing()
{
    Register_group_register::post_move_processing();
}

void
Walker_registers::change_group(Group_change_req * greq_l,
			       Voice_registers *voice_regs_l,
			       Voice_group_registers * old_group)
{
    Voice_registers *regs_p = (old_group)
	? (Voice_registers*) old_group->get_register_p(voice_regs_l)
	: new Voice_registers(greq_l->voice_l());
    Voice_group_registers * new_group_l = get_group(greq_l->newgroup_str_);
    new_group_l->add(regs_p);
    
    mtor << "processed change request";
    print();
}

Voice_group_registers *
Walker_registers::get_group(String id)
{
    for (int i=0; i < group_l_arr_.size(); i++) {
	if (group_l_arr_[i]->group_id_str_ == id)
	    return group_l_arr_[i];
    }
    Voice_group_registers *group_p = new Voice_group_registers(id);
    group_l_arr_.push(group_p);
    add(group_p);
    return group_p;
}

void
Walker_registers::terminate_register(Request_register * reg)
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
Walker_registers::try_request(Request * r)
{
    bool b = Register_group_register::try_request(r);
    if (!b) {
	Command_req * cr_l = r->command() ;
	
	if (cr_l && cr_l->groupchange()) {
	    change_group(cr_l->groupchange(), 0, 0);
	} else 
	    warning("junking request: "  + String(r->name()),
		    r->defined_ch_c_l_);
    }
    return b;
}


Staff_info
Walker_registers::get_staff_info() return inf;
{
    inf.c0_position_i_ = &walk_l_->c0_position_i_;
    inf.walk_l_ = walk_l_;
    inf.time_c_l_ = &walk_l_->time_;
    inf.rhythmic_c_l_ = walk_l_->default_grouping;
}
Paper_def*
Walker_registers::paper()const
{
    return walk_l_->staff_l_->paper();
}
