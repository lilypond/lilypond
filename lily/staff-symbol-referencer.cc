/*   
  staff-symbol-referencer.cc -- implement Staff_symbol_referencer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include <math.h>

#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "paper-def.hh"
#include "dimension-cache.hh"

Staff_symbol_referencer::Staff_symbol_referencer ()
{
  staff_sym_l_ =0;
  position_i_ =0;
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
  return (staff_sym_l_) ? staff_sym_l_->staff_line_leading_f_ : paper_l ()->get_var ("interline");
}

Real
Staff_symbol_referencer::position_f () const
{
  Real p = position_i_;
  if (staff_sym_l_ )
    {
      Graphical_element * c = common_refpoint (staff_sym_l_, Y_AXIS);
      Real y = relative_coordinate (c, Y_AXIS) - staff_sym_l_->relative_coordinate (c, Y_AXIS);

      p += 2.0 * y / staff_line_leading_f ();
    }
  return  p;
}



void
Staff_symbol_referencer::do_pre_processing ()
{
  translate_axis (position_i_ * staff_line_leading_f () /2.0, Y_AXIS);
  position_i_ =0;
}


void
Staff_symbol_referencer::set_position (int p)
{
  /*
    UGH. Use position_f() as well. 
   */
  position_i_ =   p;
  
}
