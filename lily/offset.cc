/*
  offset.cc -- implement Offset

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "string.hh"
#include "offset.hh"

String
Offset::str () const
{
  String s;
  s = String("(") + coordinate_a_[X_AXIS] + ", " + coordinate_a_[Y_AXIS] + ")";
  return s;
}
