/*
  staffsym.cc -- implement Staff_symbol

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "staffsym.hh"
#include "lookup.hh"
#include "paperdef.hh"
#include "debug.hh"

NAME_METHOD(Staff_symbol);

Staff_symbol::Staff_symbol(int l)
{
    no_lines_i_ = l;
}

void
Staff_symbol::do_print()const
{
    mtor << "lines: " << no_lines_i_;
}

Molecule*
Staff_symbol::brew_molecule_p() const
{
    Atom a  = paper()->lookup_p_->linestaff(no_lines_i_, width().length());
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
    left = p1;
    right = p2;
}
