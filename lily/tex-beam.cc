/*
  tex-beam.cc -- implement Lookup::{beam_element, beam, rule_symbol}

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

/*
  Code to generate beams for TeX
  */

#include <math.h>
#include "atom.hh"
#include "molecule.hh"
#include "tex.hh"
#include "symtable.hh"
#include "dimen.hh"
#include "debug.hh"
#include "lookup.hh"
#include "misc.hh"
#include "paper-def.hh"
#include "string-convert.hh"
#include "main.hh"


Atom
Lookup::rule_symbol (Real height, Real width) const
{
  Atom bs=(*symtables_p_)("param")->lookup ("rule");
  Array<String> args;
  args.push (print_dimen (height));
  args.push (print_dimen (width));
  bs.tex_ = substitute_args (bs.tex_,args);
  bs.dim_.x() = Interval (0,width);
  bs.dim_.y() = Interval (0,height);
  return bs;
}

Atom 
Lookup::beam(Real slope, Real width, Real thick) const
{
  
  Atom a (ps_beam (slope, width, thick));
  Real height = slope * width; 
  Real min_y = (0 <? height) - thick/2;
  Real max_y = (0 >? height) + thick/2;
  
  a.dim_[X_AXIS] = Interval(0, width);
  a.dim_[Y_AXIS] = Interval(min_y, max_y);
  return a;
}

Atom
Lookup::ps_beam (Real slope, Real width, Real thick) const
{
  String ps = "\\embeddedps{\n";
  ps += String (width) + " "+ String (slope) + " " + String (thick)
    + " draw_beam}";

  Atom s;
  s.tex_ = ps;
  return s;
}

