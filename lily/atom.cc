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
  String tex_str = sym_.tex;
  Offset off = off_;

  /* infinity checks. */
  for (int a =X_AXIS; a < NO_AXES; a++)
    {
      Axis ax = (Axis)a;
      if (abs (off[ax]) >= 100 CM)
	{
	  warning ("ridiculous dimension " + axis_name_str (ax)  + ", " 
		   +print_dimen(off[ax]));
	  off[ax] = 0.0;
	  tex_str += "\errormark"; 
	}
    }
  // whugh.. Hard coded...
  String s ("\\placebox{");
  s += print_dimen (off[Y_AXIS])+"}{";
  s += print_dimen (off[X_AXIS]) + "}{";
  s += tex_str + "}";
  return s;
}
