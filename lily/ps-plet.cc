/*
  ps-plet.cc -- implement Lookup::*plet

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include <math.h>
#include "main.hh"
#include "misc.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "dimen.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "string-convert.hh"

Atom
Lookup::plet (Real dy , Real dx, Direction dir) const
{
  String ps = "\\embeddedps{\n";
  
  ps += String_convert::double_str (dx) + " " 
    + String_convert::double_str (dy) + " "
    + String_convert::int_str ((int)dir) +
    " draw_plet}";

  Atom s;
  s.tex_ = ps;
  return s;
}
