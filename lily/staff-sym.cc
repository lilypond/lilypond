/*
  staffsym.cc -- implement Staff_symbol

  source file of the LilyPond music typesetter

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

Spanner*
Staff_symbol::do_break_at(PCol*p1, PCol*p2)const
{
    Staff_symbol *span_p=new Staff_symbol(*this);
    return span_p;
}

void
Staff_symbol::set_extent(PCol*p1, PCol*p2)
{
    assert(p1&&p2);
    left_col_l_ = p1;
    right_col_l_ = p2;
}
