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
  staff_symbol_l_ =0;
  position_f_ =0;
}

void
Staff_symbol_referencer::do_substitute_element_pointer (Score_element *o,
							Score_element*n)
{
  if (staff_symbol_l_ == o)
    {
      staff_symbol_l_ = dynamic_cast<Staff_symbol*> (n);
    }
}

int
Staff_symbol_referencer::lines_i () const
{
  return (staff_symbol_l_) ?  staff_symbol_l_->no_lines_i_ : 5;
}

void
Staff_symbol_referencer::set_staff_symbol (Staff_symbol*s)
{
  staff_symbol_l_ =s;
  add_dependency (s);
}

Staff_symbol*
Staff_symbol_referencer::staff_symbol_l () const
{
  return staff_symbol_l_;
}

Real
Staff_symbol_referencer::staff_line_leading_f () const
{
  if (staff_symbol_l_)
    return  staff_symbol_l_->staff_line_leading_f_;
  else if (pscore_l_ && paper_l ())
    paper_l ()->get_var ("interline");
 
  return 0.0;
}

Real
Staff_symbol_referencer::position_f () const
{
  Real p = position_f_;
  if (staff_symbol_l_ )
    {
      Graphical_element * c = common_refpoint (staff_symbol_l_, Y_AXIS);
      Real y = relative_coordinate (c, Y_AXIS) - staff_symbol_l_->relative_coordinate (c, Y_AXIS);

      p += 2.0 * y / staff_line_leading_f ();
    }
  return  p;
}



/*
  should use offset callback!
 */
void
Staff_symbol_referencer::do_pre_processing ()
{
  translate_axis (position_f_ * staff_line_leading_f () /2.0, Y_AXIS);
  position_f_ =0;
}


void
Staff_symbol_referencer::set_position (Real p)
{
  Real halfspace =  staff_line_leading_f ()* 0.5;
  
  translate_axis (- halfspace * position_f (), Y_AXIS);
  if (staff_symbol_l_)
    translate_axis (halfspace * p, Y_AXIS);
  else
    position_f_ =   p;
}

