/*
  staff-sym-reg.cc -- implement Staff_sym_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "staff-sym-reg.hh"
#include "staff-sym.hh"
#include "score.hh"
#include "p-col.hh"

const NO_LINES = 5;
void
Staff_sym_register::fill_staff_info(Staff_info&i)
{
    i.staff_sym_l_ = span_p_;
}

Staff_sym_register::Staff_sym_register()
{
   span_p_ = 0;
}

void
Staff_sym_register::do_creation_processing()
{
    span_p_ = new Staff_symbol(NO_LINES);
    span_p_->left_col_l_ = get_staff_info().command_pcol_l(); // ugh
}

void
Staff_sym_register::do_removal_processing()
{
    span_p_->right_col_l_ = get_staff_info().command_pcol_l();
    typeset_element(span_p_);
    span_p_ =0;
}

void
Staff_sym_register::do_process_requests()
{
    announce_element(Score_elem_info(span_p_, 0));
}


IMPLEMENT_STATIC_NAME(Staff_sym_register);
IMPLEMENT_IS_TYPE_B1(Staff_sym_register,Request_register);
ADD_THIS_REGISTER(Staff_sym_register);
