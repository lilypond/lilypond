/*
  boxes.cc -- implement Box

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "boxes.hh"
#include "varray.hh"

void
Box::translate (Offset o)
{
  x().translate (o.x ());
  y().translate (o.y ());
}

void
Box::unite (Box b)
{
  x().unite (b.x ());
  y().unite (b.y ());
}

Box::Box()
{        

}

Box::Box (Interval ix, Interval iy)
{
  x() = ix;
  y() = iy;
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
