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

Atom
Lookup::beam_element (int sidx, int widx, Real slope) const
{
  Atom bs=(*symtables_)("beamslopes")->lookup ("slope");
  
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

// ugh.. hard wired tex-code.
static int
slope_index (Real &s)
{
  if (abs (s) > 0.5) 
    {
      WARN << "beam steeper than 0.5 (" << s << ")\n";
      s = sign (s) * 0.5;
    }

  int i = int (rint (s *  20.0));

  s = i/20.0;
  if (s>0)
    return 6*i +122;
  else
    return -6 * i+ 186;
}

Atom
Lookup::rule_symbol (Real height, Real width) const
{
  Atom bs=(*symtables_)("beamslopes")->lookup ("horizontal");    
  Array<String> args;
  args.push (print_dimen (height));
  args.push (print_dimen (width));
  bs.tex_ = substitute_args (bs.tex_,args);
  bs.dim_.x() = Interval (0,width);
  bs.dim_.y() = Interval (0,height);
  return bs;
}

Atom
Lookup::beam (Real &slope, Real width) const
{        
  int sidx = slope_index (slope);
  if (!slope)
    return rule_symbol (2 PT, width);
  if (width < 2 PT) 
    {
      WARN<<"Beam too narrow. (" << print_dimen (width) <<")\n";
      width = 2 PT;
    }
  Real elemwidth = 64 PT;
  int widx = 5;

  Molecule m;
  
  while (elemwidth > width) 
    {
      widx --;
      elemwidth /= 2.0;
    }
  Real overlap = elemwidth/4;
  Real last_x = width - elemwidth;
  Real x = overlap;
  Atom elem (beam_element (sidx, widx, slope));
  Atom a (elem);
  m.add (a);
  while (x < last_x) 
    {
      a=elem;
      a.translate (Offset (x-overlap, (x-overlap)*slope));
      m.add (a);
      x += elemwidth - overlap;
    }
  a=elem;
  a.translate (Offset (last_x, (last_x) * slope));
  m.add (a);
  
  Atom ret;
  ret.tex_ = m.TeX_string();
  ret.dim_.y() = Interval (0,width*slope);
  ret.dim_.x() = Interval (0,width);
  
  return ret;
}


