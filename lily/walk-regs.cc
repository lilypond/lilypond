/*
  walkregs.cc -- implement Walker_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "debug.hh"
#include "walk-regs.hh"
#include "staff-regs.hh"
#include "staff-elem.hh"
#include "staff.hh"
#include "complex-walker.hh"
#include "staff-column.hh"
#include "score-walker.hh"
#include "bar.hh"		// needed for Bar::static_name
#include "input-register.hh"

Walker_registers::Walker_registers(Complex_walker *w)
{
    walk_l_ = w;
    Input_register * ireg_l = w->staff_l_->ireg_p_;
    if (ireg_l->name_str_ == "Staff_registers") 
	add(new Staff_registers(ireg_l));
    else {
	add(ireg_l->get_nongroup_p_arr());
    }
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


Staff_info
Walker_registers::get_staff_info() return inf;
{
    if (walk_l_->score_walk_l_)	// we get called ctors
	inf.break_allowed_b_ = walk_l_->score_walk_l_->break_allowed_b();
    inf.walk_l_ = walk_l_;
    inf.time_C_ = &walk_l_->time_;
    inf.rhythmic_C_ = walk_l_->default_grouping;
}

Paper_def*
Walker_registers::paper()const
{
    return walk_l_->staff_l_->paper();
}
