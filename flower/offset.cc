/*
  offset.cc -- implement Offset

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>

#ifndef STANDALONE
#include "string.hh"
#endif
#include "offset.hh"


#ifndef STANDALONE
String
Offset::str () const
{
  String s;
  s = String("(") + to_str (coordinate_a_[X_AXIS]) + ", " 
    + to_str (coordinate_a_[Y_AXIS]) + ")";
  return s;
}
#endif


bool
isinf_b (Real r)
{
  return (fabs (r) > 1e20);
}

/*
  free bsd fix by John Galbraith
 */
  
Offset
complex_multiply (Offset z1, Offset z2)
{
  Offset z;
  if(!isinf_b(z2[Y_AXIS]))
  {
      z[X_AXIS] = z1[X_AXIS] * z2[X_AXIS] - z1[Y_AXIS]*z2[Y_AXIS];
      z[Y_AXIS] = z1[X_AXIS] * z2[Y_AXIS] + z1[Y_AXIS] * z2[X_AXIS];
  }
  return z;
}



Offset
complex_exp (Offset o)
{
  Real s = sin (o[Y_AXIS]);
  Real c = cos (o[Y_AXIS]);
  
  Real r = exp (o[X_AXIS]);

  return Offset(r*c, r*s);
}

Real
Offset::arg () const
{
  return atan2 (y (), x());
}

/**
   euclidian vector length / complex modulus
 */
Real
Offset::length () const
{
  return sqrt (sqr (x()) + sqr (y()));
}
void
Offset::mirror (Axis a)
{
  coordinate_a_[a] = - coordinate_a_[a];
}
