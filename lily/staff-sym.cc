/*
  staffsym.cc -- implement Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "staff-sym.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "debug.hh"



Staff_symbol::Staff_symbol(int l)
{
    no_lines_i_ = l;
}

IMPLEMENT_STATIC_NAME(Staff_symbol);

void
Staff_symbol::do_print()const
{
    mtor << "lines: " << no_lines_i_;
}

Molecule*
Staff_symbol::brew_molecule_p() const
{
    Atom a  = paper()->lookup_l()->linestaff(no_lines_i_, width().length());
    return new Molecule(a);
}

void
Staff_symbol::set_extent(PCol*p1, PCol*p2)
{
    assert(p1&&p2);
    left_col_l_ = p1;
    right_col_l_ = p2;
}

Real
Staff_symbol::inter_note_f()const
{
    return paper()->internote_f();
}

int
Staff_symbol::steps_i() const
{
    return no_lines_i_*2;
}
