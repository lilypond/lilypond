/*
  staff-regs.cc -- implement Staff_registers

  source file of the GNU LilyPond music typesetter

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
#include "meter-reg.hh"
#include "staffline.hh"


Staff_registers::Staff_registers()
{
    staffline_p_ =0;
}

void
Staff_registers::typeset_element(Score_elem *elem_p)
{
    staff_elem_l_arr_.push(elem_p);
    Register_group_register::typeset_element(elem_p);
}

void
Staff_registers::typeset_breakable_item( Item * nobreak_p)
{
    staff_elem_l_arr_.push(nobreak_p);
    Register_group_register::typeset_breakable_item(nobreak_p);
}

void
Staff_registers::do_pre_move_processing()
{
    Register_group_register::do_pre_move_processing();
    group_staff_elems();
}

void
Staff_registers::group_staff_elems()
{
    for (int i=0; i <staff_elem_l_arr_.size(); i++)
	staffline_p_->add_element(staff_elem_l_arr_[i]);
    staff_elem_l_arr_.set_size(0);
}

void
Staff_registers::do_removal_processing()
{
    /* this is a "destructor type function", first do children, then self. */
    Register_group_register::do_removal_processing();
    group_staff_elems();

    staffline_p_->right_col_l_ = get_staff_info().command_pcol_l();
    Request_register::typeset_element(staffline_p_);
    staffline_p_ = 0;
}

void
Staff_registers::do_creation_processing()
{
    staffline_p_ = new Line_of_staff;
    staffline_p_->left_col_l_ = get_staff_info().command_pcol_l();

    // don't broadcast to self.
    Request_register::announce_element(Score_elem_info(staffline_p_,0));
    Register_group_register::do_creation_processing();
}


IMPLEMENT_STATIC_NAME(Staff_registers);
IMPLEMENT_IS_TYPE_B1(Staff_registers,Register_group_register);
ADD_THIS_REGISTER(Staff_registers);

