/*
  atom.cc -- implement Atom

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "symbol.hh"
#include "tex.hh"
#include "interval.hh"
#include "dimen.hh"
#include "string.hh"
#include "varray.hh"
#include "debug.hh"



void
Atom::print() const
{
    mtor << "texstring: " <<sym.tex<<"\n";    
}

Box
Atom::extent() const
{
    Box b( sym.dim);
    b.translate(off);
    return b;
}

Atom::Atom(Symbol s)
{
    sym=s;
}


String
Atom::TeX_string() const
{
    /* infinity checks. */
    assert( abs(off.x) < 100 CM);
    assert( abs(off.y) < 100 CM);
    
    // whugh.. Hard coded...
    String s("\\placebox{%}{%}{%}");
    Array<String> a;
    a.push(print_dimen(off.y));
    a.push(print_dimen(off.x));
    a.push(sym.tex);
    return substitute_args(s, a);
}
