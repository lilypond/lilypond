/*
  tex-beam.cc -- implement Lookup::{beam_element, beam, rule_symbol}

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
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
Lookup::beam(Real &slope, Real width, Real y_thick) const
{
  
  Atom a( ps_beam (slope, width, y_thick));
  Real slope_y =slope * width; 
  Real min_y = (0 <? slope_y )- y_thick/2;
  Real max_y = (0 >? slope_y) + y_thick/2;
  
  a.dim_[X_AXIS] = Interval(0, width);
  a.dim_[Y_AXIS] = Interval(min_y, max_y);
  return a;
}

Atom
Lookup::ps_beam (Real slope, Real width, Real y_thickness)const
{
  String ps = "\\embeddedps{\n";
  ps += String (width) + " "+ String (slope) + " " + String (y_thickness)
    + " draw_beam}";

  /* 
   beam parts are rarely wider than 100pt: 
   precision of 4 yields maximum (half beam spanning half a page)
   error of: 1%% * 3*72pt === 0.2pt = 0.07mm
   */
  String width_str = String_convert::precision_str (width, 4);
  String slope_str = String_convert::precision_str (slope, 4);
  String thick_str = String_convert::precision_str (y_thickness, 3);
  String name = "feta-beum-" + width_str + "-" + slope_str + "-" + thick_str;

  int i;
  while ((i = name.index_i ('.')) != -1)
    name[i]=  'x';



  Atom s;
  s.tex_ = ps;
  return s;
}
