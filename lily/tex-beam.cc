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
Lookup::beam_element (int sidx, int widx, Real slope) const
{
  String name = String("slope");
  Atom bs=(*symtables_p_)("beamslopes")->lookup (name);

  Array<String> args;
  args.push (sidx);
  args.push (widx);
  bs.tex_ = substitute_args (bs.tex_,args);
  int w = 2 << widx;
  Real width = w PT;
  bs.dim_.x() = Interval (0,width);
  bs.dim_.y() = Interval (0,width*slope);
  return bs;
}


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
  Atom a( postscript_global_b
	  ? ps_beam (slope, width, y_thick)
	  : tex_beam (slope, width));
  
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


  String mf = "\\embeddedmf{" + name + "}{\n";
  mf += "input feta-beum;\n";
  mf += "drawbeam(" + width_str + "," + slope_str + "," + thick_str + ");\n";
  mf += "end.\n";
  mf += "}\n";

  Atom s;
  s.tex_ = ps;
  if (embedded_mf_global_b)
    s.tex_ += mf;
  
  return s;
}

Atom
Lookup::tex_beam (Real &slope, Real width) const
{
  const Real MAX_SLOPE = 0.6;
  const Real SLOPES = 20.0;
  int sidx = 0;
  if (abs (slope) > MAX_SLOPE)
    {
      WARN << _("beam too steep (") << slope << ")\n";
      slope = sign (slope) * MAX_SLOPE;
    }

  sidx = int (rint (slope / MAX_SLOPE *  SLOPES));
  slope = MAX_SLOPE * sidx / SLOPES;

  Interval xdims = (*symtables_p_)("beamslopes")->lookup ("slope").dim_[X_AXIS];
  Real min_wid = xdims[LEFT];
  Real max_wid = xdims[RIGHT];
  assert(max_wid > 0);
  int widths = intlog2 (int (max_wid/min_wid)) + 1;

  if (width < min_wid)
    {
      WARN<<_("Beam too narrow. (") << print_dimen (width) <<")\n";
      width = min_wid;
    }

  Real elemwidth = max_wid;

  int widx  =widths - 1;
  Molecule m;
  while (elemwidth > width)
    {
      widx --;
      elemwidth /= 2.0;
    }
  Real overlap = elemwidth/4;
  Real last_x = width - elemwidth;
  Real x = overlap;
  Atom elem (beam_element (sidx * widths, widx, slope));
  m.add (elem);
  while (x < last_x)
    {
      Atom a(elem);
      a.translate (Offset (x-overlap, (x-overlap)*slope));
      m.add (a);
      x += elemwidth - overlap;
    }
  Atom a(elem);
  a.translate (Offset (last_x, (last_x) * slope));
  m.add (a);

  Atom ret;
  ret.tex_ = m.TeX_string();

  return ret;
}
