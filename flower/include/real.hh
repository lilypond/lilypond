/*
  real.hh -- declare Real

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REAL_HH
#define REAL_HH


#include <builtin.h>
#include <minmax.h>
#include <math.h>
#include <limits.h>

typedef double Real;
const Real infinity_f = HUGE_VAL;

inline Real
distance(Real x,Real y)
{
    return abs(x-y);
}

#endif
