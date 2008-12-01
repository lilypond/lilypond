/*
  misc.hh -- declare miscellaneous functions.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MISC_HH
#define MISC_HH

#include <cstdlib>
using namespace std;

#include "std-vector.hh"
#include "interval.hh"

double log_2 (double x);
int intlog2 (int d);

inline int
sign (int i)
{
  if (i < 0)
    return -1;
  else if (i)
    return 1;
  else return 0;
}

inline Real
linear_interpolate (Real x, Real x1, Real x2, Real y1, Real y2)
{
  return (x2 - x) / (x2 - x1) * y1
    + (x - x1) / (x2 - x1) * y2;
}

Real directed_round (Real f, Direction d);

Real peak_around (Real epsilon,  Real threshold, Real x);
Real convex_amplifier (Real standard_x, Real increase_factor, Real x);
string camel_case_to_lisp_identifier (string in);

#endif

