/*
  staff-sym-reg.cc -- implement Staff_symbol_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "staff-sym-engraver.hh"
#include "staff-sym.hh"
#include "score.hh"
#include "p-col.hh"


void
Staff_symbol_engraver::fill_staff_info (Staff_info&i)
{
  i.staff_sym_l_ = span_p_;
}

Staff_symbol_engraver::~Staff_symbol_engraver()
{
  assert (!span_p_);
}

Staff_symbol_engraver::Staff_symbol_engraver()
{
   span_p_ = 0;
}

void
Staff_symbol_engraver::do_creation_processing()
{
  span_p_ = new Staff_symbol;
  span_p_->set_bounds(LEFT,get_staff_info().command_pcol_l ());
  announce_element (Score_element_info (span_p_, 0));
}

void
Staff_symbol_engraver::do_removal_processing()
{
  Scalar l (get_property ("numberOfStaffLines", 0));
  if (l.isnum_b ())
    {
      span_p_->no_lines_i_ = l;
    }

  span_p_->set_bounds(RIGHT,get_staff_info().command_pcol_l ());
  typeset_element (span_p_);
  span_p_ =0;
}




ADD_THIS_TRANSLATOR(Staff_symbol_engraver);
