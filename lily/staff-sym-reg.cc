/*
  staff-sym-reg.cc -- implement Staff_sym_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "staff-sym-reg.hh"
#include "staff-sym.hh"
#include "score.hh"
#include "p-col.hh"

const NO_LINES = 5;

Staff_sym_register::Staff_sym_register()
{
   span_p_ = 0;
   last_mom_ =0;
}

void
Staff_sym_register::post_move_processing()
{
    if (!span_p_ && !last_mom_) {
	span_p_= new Staff_symbol(NO_LINES);
	span_p_->left_col_l_ =
	    get_staff_info().command_pcol_l()->postbreak_p_; // GUH
    }
    if (!last_mom_) {
	last_mom_ = get_staff_info().score_l()->last();
    }
}


void
Staff_sym_register::pre_move_processing()
{
    Staff_info i=get_staff_info();
    if ( span_p_ && i.when() == last_mom_) {
	span_p_->right_col_l_ = i.command_pcol_l()->prebreak_p_;
	typeset_element(span_p_);
	span_p_ =0;
    }
}

IMPLEMENT_STATIC_NAME(Staff_sym_register);
ADD_THIS_REGISTER(Staff_sym_register);
Staff_sym_register::~Staff_sym_register()
{
    assert(!span_p_);
    delete span_p_;
}
