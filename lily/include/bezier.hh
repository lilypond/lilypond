/*
  bezier.hh -- declare Bezier and Bezier_bow

  (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>
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
  void assert_sanity () const;
  void scale (Real x,Real y);
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

  /*
    Bezier curves always have 4 control points. Making this into an
    Array<> gives unnecessary overhead, and makes debugging a royal
    pain.  */

  
  Offset control_[4];
};

void scale (Array<Offset>* array, Real xscale, Real yscale);
void rotate (Array<Offset>* array, Real phi);
void translate (Array<Offset>* array, Offset o);

#endif // BEZIER_HH

