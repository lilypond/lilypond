/*
  tex-slur.cc -- implement Lookup::*slur

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <math.h>
#include "misc.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "dimen.hh"
#include "debug.hh"
#include "paper-def.hh"


static char
direction_char (Direction y_sign)
{
  char c='#';
  switch (y_sign)
    {
    case DOWN:
      c = 'd';
      break;
    case CENTER:
      c = 'h';
      break;
    case UP:
      c = 'u';
      break;
    default:
      assert (false);
    }
  return c;
}

Symbol
Lookup::half_slur_middlepart (Real &dx, Direction dir) const
{
  // todo
  if (dx >= 400 PT)
    {
      WARN<<"halfslur too large" <<print_dimen (dx)<< "shrinking (ugh)\n";
      dx = 400 PT;
    }
  int widx = int (floor (dx / 4.0));
  dx = widx * 4.0;
  if (widx) widx --;
  else 
    {
      WARN <<  "slur too narrow\n";
    }

  Symbol s;
  
  s.dim.y() = Interval (min (0,0), max (0,0)); // todo
  s.dim.x() = Interval (0,dx);

  String f =  String ("\\hslurchar");
  f += direction_char (CENTER);

  int idx = widx;
  if (dir < 0)
    idx += 128;

  assert (idx < 256);

  f+=String ("{") + String (idx) + "}";
  s.tex = f;
  Atom a (s);
  a.translate (dx/2, X_AXIS);
  s.tex = a.TeX_string();

  return s;
}

Symbol
Lookup::half_slur (int dy, Real &dx, Direction dir, int xpart) const
{
  Real orig_dx = dx;
  if (!xpart)
    return half_slur_middlepart (dx, dir);

  int widx;
  	 	
  if (dx >= 96 PT) 
    {
      WARN << "Slur half too wide." << print_dimen (orig_dx) << " shrinking (ugh)\n";
      dx =  96 PT;
    }

  widx = int (rint (dx/12.0));
  dx = widx*12.0;
  if (widx)
    widx --;
  else 
    {
      WARN <<  "slur too narrow " << print_dimen (orig_dx)<<"\n";
    }
	
  Symbol s;
  s.dim.x() = Interval (0,dx);
  s.dim.y() = Interval (min (0,dy), max (0,dy));


  String f = String ("\\hslurchar");

  f+= direction_char (dir);

  int hidx = dy;
  if (hidx <0)
    hidx = -hidx;
  hidx --;
  int idx =-1;

  idx = widx * 16 + hidx;
  if (xpart < 0)
    idx += 128;
  
  assert (idx < 256);
  f+=String ("{") + String (idx) + "}";

  
  s.tex = f;
 
  return s;
}

Symbol
Lookup::slur (int dy , Real &dx, Direction dir) const
{
  
  assert (abs (dir) <= 1);
  if  (dx < 0)
    {
      warning ("Negative slur/tie length: " + print_dimen (dx));
      dx = 4.0 PT;
    }
  Direction y_sign = (Direction) sign (dy);

  bool large = abs (dy) > 8;

  if (y_sign) 
    {
      large |= dx>= 4*16 PT;
    }
  else
    large |= dx>= 4*54 PT;
  
  if (large) 
    {
      return big_slur (dy, dx, dir);
    }
  Real orig_dx = dx;
  int widx = int (floor (dx/4.0)); // slurs better too small..
  dx = 4.0 * widx;
  if (widx)
    widx --;
  else 
    {
      WARN <<  "slur too narrow: " << print_dimen (orig_dx) << "\n";
    }

  int hidx = dy;
  if (hidx <0)
    hidx = -hidx;
  hidx --; 
  if (hidx > 8) 
    {
      WARN<<"slur to steep: " << dy << " shrinking (ugh)\n";
    }
  
  Symbol s;
  s.dim.x() = Interval (0,dx);
  s.dim.y() = Interval (min (0,dy), max (0,dy));

  String f = String ("\\slurchar") + String (direction_char (y_sign));

  int idx=-1;
  if (y_sign) {	
    idx = hidx * 16 + widx;
    if (dir < 0)
      idx += 128;
  }
  else 
    {
      if (dx >= 4*54 PT) 
	{
	  WARN << "slur too wide: " << print_dimen (dx) <<
	    " shrinking (ugh)\n";
	  dx = 4*54 PT;
	}
      idx = widx;
      if (dir < 0)
	idx += 54;		
    }
  
  assert (idx < 256);
  f+=String ("{") + String (idx) + "}";
  s.tex = f;

  Atom a (s);
  a.translate (dx/2, X_AXIS);
  s.dim = a.extent();
  s.tex = a.TeX_string();
  return s;    
}

Symbol
Lookup::big_slur (int dy , Real &dx, Direction dir) const
{
  if (dx < 24 PT) 
    {
      warning ("big_slur too small " + print_dimen (dx) + " (stretching)");
      dx = 24 PT;
    }
  
  Real slur_extra =abs (dy)  /2.0 + 2; 
  int l_dy = int (Real (dy)/2 + slur_extra*dir);
  int r_dy =  dy - l_dy;

  Real internote_f = paper_l_->internote_f();
  Real left_wid = dx/4.0;
  Real right_wid = left_wid;

  Atom l = half_slur (l_dy, left_wid, dir, -1);
  Atom r = half_slur (r_dy, right_wid, dir, 1);
  Real mid_wid = dx - left_wid - right_wid;

  Atom m = half_slur (0, mid_wid, dir, 0);

  Molecule mol;
  mol.add (l);
  Atom a (m);
  a.translate (slur_extra * internote_f, Y_AXIS);
  mol.add_at_edge (X_AXIS, RIGHT, m);
  mol.add_at_edge (X_AXIS, RIGHT, r);
  mol.translate (l_dy * internote_f, Y_AXIS);
  Symbol s;
  s.tex = mol.TeX_string();
  s.dim = mol.extent();
  return s;
}


