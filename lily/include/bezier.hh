/*
  bezier.hh -- declare Bezier and Bezier_bow

  (c) 1998--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef BEZIER_HH
#define BEZIER_HH

#include "interval.hh"
#include "offset.hh"
#include "polynomial.hh"

/**
   Simple bezier curve
*/
class Bezier
{
public:
  void assert_sanity () const;
  void scale (Real x, Real y);
  void reverse ();
  void rotate (Real);
  void translate (Offset);

  Real get_other_coordinate (Axis a, Real x) const;
  vector<Real> solve_point (Axis, Real coordinate) const;
  vector<Real> solve_derivative (Offset) const;
  Interval extent (Axis) const;
  Interval control_point_extent (Axis) const;
  
  Polynomial polynomial (Axis)const;
  Offset curve_point (Real t) const;
  Real curve_coordinate (Real t, Axis) const;

  static const int CONTROL_COUNT = 4;

  /*
    Bezier curves always have 4 control points. Making this into an
    vector<> gives unnecessary overhead, and makes debugging a royal
    pain.  */

  Offset control_[4];
};

void scale (vector<Offset> *array, Real xscale, Real yscale);
void rotate (vector<Offset> *array, Real phi);
void translate (vector<Offset> *array, Offset o);

Bezier slur_shape (Real width, Real height_limit,
		   Real height_proportion);
Real slur_height (Real width, Real height_limit, Real height_proportion);
void get_slur_indent_height (Real *indent, Real *height, Real width, Real h_inf, Real r_0);

#endif // BEZIER_HH

