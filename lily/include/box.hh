/*
  some 2D geometrical concepts
*/

#ifndef BOXES_HH
#define BOXES_HH

#include "interval.hh"
#include "offset.hh"
#include "smobs.hh"

class Box
{
  Interval interval_a_[NO_AXES];
  DECLARE_SIMPLE_SMOBS (Box);
public:
  Interval &x () {return interval_a_[X_AXIS]; }
  Interval &y (){ return interval_a_[Y_AXIS]; }
  Interval x () const{ return interval_a_[X_AXIS]; }
  Interval y () const{return interval_a_[Y_AXIS]; }
  Interval operator [] (Axis a) const;
  Interval &operator [] (Axis a);

  Offset center () const;

  void translate (Offset o);

  /// smallest box enclosing #b#
  void set_empty ();
  void add_point (Offset);
  void widen (Real x, Real y);
  void scale (Real r);
  void unite (Box b);
  Box ();
  Box (Interval ix, Interval iy);
};

#endif
