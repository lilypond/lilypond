/*
  real.hh -- declare Real

  source file of the Flower Library

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef REAL_HH
#define REAL_HH

#include <climits>

typedef double Real;
extern const Real infinity_f;
using namespace std;

template<class T> inline T abs (T x)
{
  return x > 0 ? x : -x;
}

template<class T> inline int sign (T x)
{
  if (x)
    return x > T (0) ? 1 : -1;
  return 0;
}


template<class T> inline T sqr (T x)
{
  return x * x;
}


inline Real
distance (Real x, Real y)
{
  return abs (x - y);
}

#endif
