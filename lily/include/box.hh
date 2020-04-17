/*
  some 2D geometrical concepts
*/

#ifndef BOXES_HH
#define BOXES_HH

#include "interval.hh"
#include "offset.hh"
#include "smobs.hh"

class Box : public Simple_smob<Box>
{
public:
  static const char *const type_p_name_;
private:
  Interval interval_a_[NO_AXES];
public:
  Interval &x () {return interval_a_[X_AXIS]; }
  Interval &y () { return interval_a_[Y_AXIS]; }
  Interval x () const { return interval_a_[X_AXIS]; }
  Interval y () const {return interval_a_[Y_AXIS]; }
  Interval operator [] (Axis a) const;
  Interval &operator [] (Axis a);
  Real area () const;
  bool is_empty () const;
  bool is_empty (Axis a) const;

  Offset center () const;

  void translate (Offset o);

  void set_empty ();
  void add_point (Offset);
  void widen (Real x, Real y);
  void scale (Real r);

  /// smallest box enclosing `this` and `b`
  void unite (Box b);
  void intersect (Box b);
  void print ();
  Box ();
  Box (Interval ix, Interval iy);
};

#endif
