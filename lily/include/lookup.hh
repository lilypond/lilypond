/*
  lookup.hh -- declare Lookup

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LOOKUP_HH
#define LOOKUP_HH

#include "string.hh"
#include "stencil.hh"
#include "flower-proto.hh"
#include "direction.hh"
#include "box.hh"

struct Lookup
{
  static Stencil dot (Offset p, Real radius);
  static Stencil bracket (Axis a, Interval iv, Real thick, Real protude, Real blot);
  static Stencil accordion (SCM arg, Real interline_f, Font_metric*fm);
  static Stencil round_filled_polygon (Array<Offset> points, Real blotdiameter);
  static Stencil frame (Box b, Real thick, Real blot);
  static Stencil slur (Bezier controls, Real cthick, Real thick);
  static Stencil bezier_sandwich (Bezier top_curve, Bezier bottom_curve);
  static Stencil beam (Real slope, Real width, Real thick, Real blot);
  static Stencil dashed_slur (Bezier, Real thick, Real dash_period, Real dash_fraction);
  static Stencil blank (Box b);
  static Stencil filled_box (Box b);
  static Stencil round_filled_box (Box b, Real blotdiameter);
  static Stencil repeat_slash (Real w, Real slope, Real th);
  static Stencil horizontal_line (Interval w, Real th);
  static Stencil triangle (Interval, Real, Real);
};

#endif // LOOKUP_HH
