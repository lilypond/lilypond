/*
  staffsym.cc -- implement Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "staff-symbol.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "debug.hh"
#include "item.hh"


Staff_symbol::Staff_symbol ()
{
  no_lines_i_ = 5;
  staff_line_leading_f_ = 5.0 PT;
}

void
Staff_symbol::do_print() const
{
#ifndef NPRINT
  Spanner::do_print();
  DOUT << "lines: " << no_lines_i_;
#endif
}

Interval
Staff_symbol::do_height() const
{
  Interval i =Interval (0, staff_line_leading_f_ * (no_lines_i_-1));
  i += - i.center ();
  return i;
}

Molecule*
Staff_symbol::do_brew_molecule_p() const
{
  Graphical_element * common
    = spanned_drul_[LEFT]->common_refpoint (spanned_drul_[RIGHT], X_AXIS);

#if 0
  Interval r =  spanned_drul_[RIGHT]->extent (X_AXIS);
  Interval l =  spanned_drul_[LEFT]->extent (X_AXIS);
  
  Real left_shift =l.empty_b () ? 0.0: l[LEFT];
  Real right_shift =r.empty_b () ? 0.0: r[RIGHT];  
#endif
  Real width =
    // right_shift     - left_shift
    + spanned_drul_[RIGHT]->relative_coordinate (common , X_AXIS)
    - spanned_drul_[LEFT]->relative_coordinate (common, X_AXIS)
    ;

  Real t = paper_l ()->get_var ("rulethickness");
  Molecule rule  = lookup_l ()->filledbox (Box (Interval (0,width),
						Interval (-t/2, t/2)));

  Real height = (no_lines_i_-1) * staff_line_leading_f_ /2;
  Molecule * m = new Molecule;
  for (int i=0; i < no_lines_i_; i++)
    {
      Molecule a (rule);
      a.translate_axis (height - i * staff_line_leading_f_, Y_AXIS);
      m->add_molecule (a);
    }

  //  m->translate_axis (left_shift, X_AXIS);
  return m;
}


int
Staff_symbol::steps_i() const
{
  return no_lines_i_*2;
}
