/*
  interval.cc -- instantiate Interval_t<Real>

  source file of the Flower Library

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "interval.hh"
#include "real.hh"
#include "interval.tcc"

Real
Interval_t<Real>::infinity () 
{
  return HUGE_VAL;
}

String
Interval_t<Real>::T_to_string (Real r)
{
  return to_string (r);
}


int
Interval_t<int>::infinity () 
{
  return INT_MAX;
}

String
Interval_t<int>::T_to_string (int i)
{
  return to_string (i);
}

template INTERVAL__INSTANTIATE (int);
template INTERVAL__INSTANTIATE (Real);

