/*
  staffsym.cc -- implement Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "staff-sym.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "debug.hh"
#include "item.hh"


Staff_symbol::Staff_symbol ()
{
  no_lines_i_ = 5;
  interline_f_ =  0 PT;
}


IMPLEMENT_IS_TYPE_B1(Staff_symbol,Spanner);

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
  int n = no_lines_i_ -1;
//  return 2* inter_note_f () * Interval (-n, n);
  return inter_note_f () * Interval (-n, n);
}

Molecule*
Staff_symbol::brew_molecule_p() const
{
  Real w = width ().length ();
  Real left_dx = -spanned_drul_[LEFT]->extent (X_AXIS)[LEFT];
  Real right_dx = spanned_drul_[RIGHT]->extent (X_AXIS)[RIGHT];
  //  w += right_dx+ left_dx;

  
  Paper_def * p = paper();
  Atom rule  = lookup_l ()->rule_symbol (p->get_var ("rulethickness"),
					 w);
  Real height = (no_lines_i_-1) * inter_note_f();
  Molecule * m = new Molecule;
  for (int i=0; i < no_lines_i_; i++)
    {
      Atom a (rule);
      a.translate_axis (height - i * inter_note_f()*2, Y_AXIS);
      m->add_molecule (a);
    }

  //  m->translate_axis (-left_dx, X_AXIS);
  return m;
}

Real
Staff_symbol::inter_note_f() const
{
  if (interline_f_)
    return interline_f_/2;

  return paper()->internote_f ();
}

int
Staff_symbol::steps_i() const
{
  return no_lines_i_*2;
}
