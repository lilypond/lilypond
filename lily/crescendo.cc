/*
  crescendo.cc -- implement Crescendo

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "crescendo.hh"
#include "lookup.hh"
#include "paper-def.hh"

Crescendo::Crescendo(int s)
{
    staff_size_i_ = s;
    grow_dir_i_ =0;
    dir_i_ = -1 ;
}

Spanner*
Crescendo::do_break_at(PCol*, PCol*)const
{
    return new Crescendo(*this);
}


Molecule*
Crescendo::brew_molecule_p() const return m_p ;
{
    m_p = new Molecule;
    Real w_f = width().length();
    Symbol s( paper()->lookup_l()->hairpin(w_f, grow_dir_i_ < 0) );
    m_p->add(Atom(s));
    int pos = (dir_i_ >0) ? staff_size_i_ + 4 : - 4 ;
    m_p->translate(Offset(0,pos * paper()->internote()));
}
