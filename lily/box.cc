/*
  box.cc -- implement Box

  source file of the GNU LilyPond music typesetter

  (c) 1996--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "box.hh"
#include "array.hh"

void
Box::translate (Offset o)
{
  for (Axis i=X_AXIS; i < NO_AXES; incr (i))
    interval_a_[i] += o[i];
}

void
Box::unite (Box b)
{
  for (Axis i=X_AXIS; i < NO_AXES; incr (i))
    interval_a_[i].unite (b[i]);
}

/**
  Initialize to empty.
 */
Box::Box ()
{        
}

void
Box::set_empty ()
{
  interval_a_[X_AXIS].set_empty ();
  interval_a_[Y_AXIS].set_empty (); 
}

Box::Box (Interval ix, Interval iy)
{
  x () = ix;
  y () = iy;
}

Interval &
Box::operator[] (Axis a)
{
  return interval_a_[a];
}

Interval
Box::operator[] (Axis a)const
{
  return interval_a_[a];
}

void
Box::scale (Real s)
{
  interval_a_[X_AXIS] *= s;
  interval_a_[Y_AXIS] *= s;
}
