#ifndef MISC_HH
#define MISC_HH

#include <cstdlib>

#include "array.hh"
#include "interval.hh"

double log_2 (double x) ;
int intlog2 (int d);

inline int
sign (int i)
{
  if (i<0)
    return -1;
  else if (i)
    return 1;
  else return 0;
}



inline Real
linear_interpolate (Real x, Real x1, Real x2,  Real y1, Real  y2)
{
  return (x2 - x) / (x2 - x1) * y1 +
    (x - x1) / (x2 - x1) * y2 ;
}



#endif

