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

  void assert_sanity () const;
  void flip (Axis);
  void reverse ();
  void rotate (Real);
  void translate (Offset);

  Real get_other_coordinate (Axis a, Real x) const;
  Array<Real> solve_point (Axis, Real coordinate) const;
  Array<Real> solve_derivative (Offset) const;
  Interval extent (Axis)const;
  Polynomial polynomial (Axis)const;
  Offset curve_point (Real t) const;

  static const int CONTROL_COUNT = 4;
  Array<Offset> control_;
};

void flip (Array<Offset>* arr_p, Axis a);
void rotate (Array<Offset>* arr_p, Real phi);
void translate (Array<Offset>* arr_p, Offset o);

#endif // BEZIER_HH

