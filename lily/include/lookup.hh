/*
  lookup.hh -- declare Lookup

  source file of the GNU LilyPond music typesetter

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LOOKUP_HH
#define LOOKUP_HH

#include "string.hh"
#include "molecule.hh"
#include "flower-proto.hh"
#include "direction.hh"
#include "box.hh"

struct Lookup
{
  static Molecule dot (Offset p, Real radius);
  static Molecule bracket (Axis a, Interval iv, Real thick, Real protude, Real blot);
  static Molecule accordion (SCM arg, Real interline_f, Font_metric*fm);
  static Molecule round_filled_polygon (Array<Offset> points, Real blotdiameter);
  static Molecule frame (Box b, Real thick, Real blot);
  static Molecule slur (Bezier controls, Real cthick, Real thick);
  static Molecule bezier_sandwich (Bezier top_curve, Bezier bottom_curve);
  static Molecule beam (Real slope, Real width, Real thick, Real blot);
  static Molecule dashed_slur (Bezier, Real thick, Real dash);
  static Molecule blank (Box b);
  static Molecule filled_box (Box b);
  static Molecule round_filled_box (Box b, Real blotdiameter);
  static Molecule repeat_slash (Real w, Real slope, Real th);
  static Molecule line (Real th, Offset from, Offset to);
  static Molecule horizontal_line (Interval w, Real th);
  static Molecule triangle (Interval, Real, Real);
};

#endif // LOOKUP_HH
