/*
    some 2D geometrical concepts
*/

#ifndef BOXES_HH
#define BOXES_HH

#include "flower-proto.hh"
#include "real.hh"
#include "interval.hh"
#include "offset.hh"
#include "axes.hh"


struct Box {
  Interval interval_a_[NO_AXES];
    
  Interval &x() {return interval_a_[X_AXIS]; }
  Interval &y(){ return interval_a_[Y_AXIS]; }
  Interval x() const{ return interval_a_[X_AXIS]; }
  Interval y() const{return interval_a_[Y_AXIS]; }
  Interval operator[](Axis a) const;
  Interval &operator[] (Axis a);
    
  void translate (Offset o);
  /// smallest box enclosing #b#
  void set_empty ();
  void unite (Box b);
  Box();
  Box (Interval ix, Interval iy);
};


#endif
