/*
  real.hh -- declare Real

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REAL_HH
#define REAL_HH


#include <math.h>
#include <limits.h>

typedef double Real;
const Real infinity_f = HUGE_VAL;

template<class T> inline T abs (T x)
{
  return x > 0 ? x : -x;
}

template<class T> inline int sign (T x)
{
  if (x)
    return x > 0 ? 1 : -1;
  return 0;
}

template<class T> inline T max (T x, T y)
{
  return x >? y;
}

template<class T> inline T sqr (T x)
{
  return x*x;
}


template<class T> inline T min (T x, T y)
{
  return x <? y;
}

inline Real
distance (Real x,Real y)
{
    return abs (x-y);
}

#endif
