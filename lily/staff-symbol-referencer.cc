/*   
  staff-symbol-referencer.cc -- implement Staff_symbol_referencer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "paper-def.hh"

Staff_symbol_referencer::Staff_symbol_referencer ()
{
  staff_sym_l_ =0;
}

void
Staff_symbol_referencer::do_substitute_element_pointer (Score_element *o, Score_element*n)
{
  if (staff_sym_l_ == o)
    {
      staff_sym_l_ = dynamic_cast<Staff_symbol*> (n);
    }
}

int
Staff_symbol_referencer::lines_i () const
{
  return (staff_sym_l_) ?  staff_sym_l_->no_lines_i_ : 5;
}

void
Staff_symbol_referencer::set_staff_symbol (Staff_symbol*s)
{
  staff_sym_l_ =s;
  add_dependency (s);
}

Staff_symbol*
Staff_symbol_referencer::staff_symbol_l () const
{
  return staff_sym_l_;
}

Real
Staff_symbol_referencer::staff_line_leading_f () const
{
  return (staff_sym_l_) ? staff_sym_l_->staff_line_leading_f_ : paper_l ()->get_realvar (interline_scm_sym);
}
