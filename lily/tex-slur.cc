/*
  tex-slur.cc -- implement Lookup::*slur

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
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

Atom
Lookup::half_slur_middlepart (Real &dx, Direction dir) const
{
  // todo
  if (dx >= 400 PT)
    {
      WARN<<_("halfslur too large") <<print_dimen (dx)<< _("shrinking (ugh)\n");
      dx = 400 PT;
    }
  int widx = int (floor (dx / 4.0));
  dx = widx * 4.0;
  if (widx) widx --;
  else
    {
      WARN <<  _("slur too narrow\n");
    }

  Atom s;

  s.dim_[Y_AXIS] = Interval (min (0, 0), max (0, 0)); // todo
  s.dim_[X_AXIS] = Interval (-dx/2, dx/2);

  String f =  String ("\\hslurchar");
  f += direction_char (CENTER);

  int idx = widx;
  if (dir < 0)
    idx += 128;

  assert (idx < 256);

  f +=String ("{") + String (idx) + "}";
  s.tex_ = f;
  s.translate_axis (dx/2, X_AXIS);

  return s;
}

/*
   The halfslurs have their center at the end pointing away from the notehead.
   This lookup translates so that width() == [0, w]
 */

Atom
Lookup::half_slur (int dy, Real &dx, Direction dir, int xpart) const
{
  Real orig_dx = dx;
  if (!xpart)
    return half_slur_middlepart (dx, dir);

  int widx;

  if (dx >= 96 PT)
    {
      WARN << _("Slur half too wide.") << print_dimen (orig_dx) << _(" shrinking (ugh)\n");
      dx =  96 PT;
    }

  widx = int (rint (dx/12.0));
  dx = widx*12.0;
  if (widx)
    widx --;
  else
    {
      WARN <<  _("slur too narrow ") << print_dimen (orig_dx)<<"\n";
    }

  Atom s;
  s.dim_[X_AXIS] = Interval (0, dx);
  s.dim_[Y_AXIS] = Interval (min (0, dy), max (0, dy));


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

  s.tex_ = f;

  return s;
}

Atom
Lookup::ps_slur (Real dy , Real dx, Real ht, Real dir) const
{
  String ps = "\\embeddedps{\n";
  
  ps += String_convert::double_str (dx) + " " 
    + String_convert::double_str (dy) + " "
    + String_convert::double_str (ht) + " "
    + String_convert::double_str (dir) +
    " draw_slur}";

  /*
   slurs are rarely wider than 100pt: 
   precision of 3 yields maximum (slur spanning page)
   error of: 1%% * 6*72pt === 0.4pt = 0.14 mm
   */
  String dx_str = String_convert::precision_str (dx, 4);
  String dy_str = String_convert::precision_str (dy, 3);
  String ht_str = String_convert::precision_str (ht, 3);
  String dir_str = String_convert::int_str ((int)dir);
  String name = "feta-sleur-" + dx_str + "-" + dy_str + "-" + ht_str + "-" + dir_str;
  int i = name.index_i ('.');
  while (i != -1)
    {
      *(name.ch_l () + i) = 'x';
      i = name.index_i ('.');
    }

  String mf = "\\embeddedmf{" + name + "}{\n";
  mf += "mode_setup;\n";
  mf += "staffsize\\#:=" 
    + String_convert::int_str ((int)paper_l_->get_var ("barsize"), "%d")
    + "pt\\#;\n";
  mf += "interline#:=staffsize#/4;\n";
  mf += "stafflinethickness#:=0.1interline#;\n";
  mf += "input feta-sleur;\n";
  mf += "slurchar(" + dx_str + "," + dy_str + "," + ht_str + "," + dir_str + ");\n";
  mf += "end.\n";
  mf += "}\n";

  Atom s;
  s.tex_ = ps + mf;
  return s;
}

Atom
Lookup::tex_slur (int dy , Real &dx, Direction dir) const
{
  assert (abs ((int)dir) <= 1);
  Atom s;
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
      s = big_slur (dy, dx, dir);
      return s;
    }
  Real orig_dx = dx;
  int widx = int (floor (dx/4.0)); // slurs better too small..
  dx = 4.0 * widx;
  if (widx)
    widx --;
  else
    {
      WARN <<  _("slur too narrow: ") << print_dimen (orig_dx) << "\n";
    }

  int hidx = dy;
  if (hidx <0)
    hidx = -hidx;
  hidx --;
  if (hidx > 8)
    {
      WARN<<_("slur to steep: ") << dy << _(" shrinking (ugh)\n");
    }

  String f = String ("\\slurchar") + String (direction_char (y_sign));

  int idx=-1;
  if (y_sign)
    {
      idx = hidx * 16 + widx;
      if (dir < 0)
	idx += 128;
    }
  else
    {
      if (dx >= 4*54 PT)
	{
	  WARN << _("slur too wide: ") << print_dimen (dx) <<
	    _(" shrinking (ugh)\n");
	  dx = 4*54 PT;
	}
      idx = widx;
      if (dir < 0)
	idx += 54;
    }

  assert (idx < 256);
  f+=String ("{") + String (idx) + "}";
  s.tex_ = f;
  s.translate_axis (dx/2, X_AXIS);
  return s;
}

Atom
Lookup::big_slur (int dy , Real &dx, Direction dir) const
{
  if (dx < 24 PT)
    {
      warning (_("big_slur too small ") + print_dimen (dx) + _(" (stretching)"));
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

  Molecule mol;
  mol.add (l);
  Atom a (half_slur (0, mid_wid, dir, 0));
  mol.add_at_edge (X_AXIS, RIGHT, a);
  mol.add_at_edge (X_AXIS, RIGHT, r);

  mol.translate_axis (l_dy * internote_f, Y_AXIS);
  Atom s;
  s.tex_ = mol.TeX_string();
  s.dim_ = mol.extent();
  return s;
}


Atom
Lookup::slur  (Real &dy_f , Real &dx, Real ht, Direction dir) const
{
  if  (dx < 0)
    {
      warning (_("Negative slur/tie length: ") + print_dimen (dx));
      dx = 4.0 PT;
    }
  Atom s;
  
  if (postscript_global_b)
    s = ps_slur (dy_f, dx, ht, dir);
  else
    {
      Real nh =  paper_l_->internote_f ();
      int dy_i = (int) rint(dy_f / nh);

      s = tex_slur (dy_i, dx, dir);
      dy_f = dy_i *  nh;
    }
  
  s.dim_[X_AXIS] = Interval (0, dx);
  s.dim_[Y_AXIS] = Interval (0 <? dy_f,  0 >? dy_f);
  return s;
}
