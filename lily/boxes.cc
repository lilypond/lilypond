/*
  boxes.cc -- implement Box

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "boxes.hh"
#include "varray.hh"


Box::Box()
{        
}

Box::Box (Interval ix, Interval iy)
{
  x() = ix;
  y() = iy;
}

