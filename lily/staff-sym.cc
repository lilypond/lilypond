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
  Atom a  = paper()->lookup_l ()->linestaff (no_lines_i_, paper ()->interline_f(), width ().length ());
  return new Molecule (a);
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
