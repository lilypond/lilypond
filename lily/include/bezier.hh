/*
  bezier.hh -- declare Bezier and Bezier_bow

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef BEZIER_HH
#define BEZIER_HH


#include "real.hh"
#include "drul-array.hh"
#include "interval.hh"
#include "axes.hh"
#include "offset.hh"
#include "array.hh"
#include "polynomial.hh"

/**
  Simple bezier curve
 */
class Bezier
{
public:
  Bezier ();

  void rotate (Real);
  void translate (Offset);
  void flip (Axis);
  void check_sanity () const;
  Real get_other_coordinate (Axis a, Real x) const;
  Array<Real> solve_point (Axis, Real coordinate) const;
  Array<Real> solve_derivative (Offset) const;
  Interval extent (Axis)const;
  Polynomial polynomial (Axis)const;
  Offset curve_point (Real t) const;

  void reverse ();

  static const int CONTROL_COUNT = 4;
  Offset control_[CONTROL_COUNT];
};


#endif // BEZIER_HH

