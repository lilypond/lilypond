#ifndef MISC_HH
#define MISC_HH

#include <stdlib.h>

#include "real.hh"
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

Interval quantise_iv (Array<Real> positions, Real x);

#endif

