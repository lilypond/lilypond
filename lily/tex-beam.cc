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
  Atom bs=(*symtables_p_)("beamslopes")->lookup ("horizontal");
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
  if (postscript_global_b)
    {
      return ps_beam (slope, width, y_thick);
    }
  else
    return tex_beam(slope, width);
}

Atom
Lookup::ps_beam (Real  slope, Real width, Real y_thickness)const
{
  Atom ret;
  String ps(String (width) + " "+ String(slope) 
	    + " " + String(y_thickness) + " draw_beam");
  ret.tex_ = String("\\embeddedps{" + ps + "}");
  ret.dim_[X_AXIS] = Interval(0, width);
  ret.dim_[Y_AXIS] = Interval(0, slope * width + y_thickness);
  return ret;
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
  ret.dim_.y() = Interval (0,width*slope);
  ret.dim_.x() = Interval (0,width);

  return ret;
}
