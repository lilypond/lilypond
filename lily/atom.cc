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
#ifndef NPRINT
  DOUT << "texstring: " <<sym_.tex<<"\n";    
#endif
}

Box
Atom::extent() const
{
  Box b (sym_.dim);
  b.translate (off_);
  return b;
}

Atom::Atom (Symbol s)
{
  sym_=s;
}


String
Atom::TeX_string() const
{
  /* infinity checks. */
  assert (abs (off_.x()) < 100 CM);
  assert (abs (off_.y()) < 100 CM);
  
  // whugh.. Hard coded...
  String s ("\\placebox{%}{%}{%}");
  Array<String> a;
  a.push (print_dimen (off_.y()));
  a.push (print_dimen (off_.x()));
  a.push (sym_.tex);
  return substitute_args (s, a);
}
