/*
  staff-sym-reg.cc -- implement Staff_symbol_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "staff-sym-engraver.hh"
#include "staff-symbol.hh"
#include "score.hh"
#include "paper-column.hh"
#include "staff-symbol-referencer.hh"
#include "paper-def.hh"

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
  SCM l (get_property ("numberOfStaffLines", 0));
  if (gh_number_p(l))
    {
      span_p_->no_lines_i_ = gh_scm2int (l);
    }

  SCM sz (get_property ("staffLineLeading", 0));
  if (gh_number_p(sz))
    {
      span_p_->staff_line_leading_f_ = gh_scm2double (sz);
    }
  else
    {
      span_p_->staff_line_leading_f_ = paper_l ()->get_realvar (interline_scm_sym);
    }
  span_p_->set_bounds(RIGHT,get_staff_info().command_pcol_l ());
  typeset_element (span_p_);
  span_p_ =0;
}

void
Staff_symbol_engraver::acknowledge_element (Score_element_info s)
{
  if (Staff_symbol_referencer * st = dynamic_cast<Staff_symbol_referencer*> (s.elem_l_))
    {
      st->set_staff_symbol (span_p_);      
    }
}


ADD_THIS_TRANSLATOR(Staff_symbol_engraver);
