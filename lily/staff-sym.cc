/*
  staffsym.cc -- implement Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "staff-sym.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "debug.hh"



Staff_symbol::Staff_symbol (int l)
{
  no_lines_i_ = l;
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

Molecule*
Staff_symbol::brew_molecule_p() const
{
  Paper_def * p = paper();
  Atom rule  = p->lookup_l ()->rule_symbol (p->get_var ("rule_thickness"), 
					    width ().length ());
  Real inter = p->interline_f ();
  Real height = (no_lines_i_-1) * inter/2;
  Molecule * m = new Molecule;
  for (int i=0; i < no_lines_i_; i++)
    {
      Atom a (rule);
      a.translate_axis (height - i * inter, Y_AXIS);
      m->add (a);
    }

  return m;
}

Real
Staff_symbol::inter_note_f() const
{
  return paper()->internote_f ();
}

int
Staff_symbol::steps_i() const
{
  return no_lines_i_*2;
}
