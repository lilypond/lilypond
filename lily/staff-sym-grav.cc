/*
  staff-sym-reg.cc -- implement Staff_sym_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "staff-sym-grav.hh"
#include "staff-sym.hh"
#include "score.hh"
#include "p-col.hh"

const NO_LINES = 5;
void
Staff_sym_engraver::fill_staff_info (Staff_info&i)
{
    i.staff_sym_l_ = span_p_;
}

Staff_sym_engraver::~Staff_sym_engraver()
{
    assert (!span_p_);
}

Staff_sym_engraver::Staff_sym_engraver()
{
   span_p_ = 0;
}

void
Staff_sym_engraver::do_creation_processing()
{
    span_p_ = new Staff_symbol (NO_LINES);
    span_p_->left_col_l_ = get_staff_info().command_pcol_l (); // ugh
    announce_element (Score_elem_info (span_p_, 0));
}

void
Staff_sym_engraver::do_removal_processing()
{
    span_p_->right_col_l_ = get_staff_info().command_pcol_l ();
    typeset_element (span_p_);
    span_p_ =0;
}



IMPLEMENT_IS_TYPE_B1(Staff_sym_engraver,Engraver);
ADD_THIS_ENGRAVER(Staff_sym_engraver);
