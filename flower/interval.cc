/*
  interval.cc -- instantiate Interval_t<Real>

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
Interval_t<Real>::T_to_str (Real r)
{
  return to_str (r);
}


int
Interval_t<int>::infinity () 
{
  return INT_MAX;
}

String
Interval_t<int>::T_to_str (int i)
{
  return to_str (i);
}

template INTERVAL__INSTANTIATE (int);
template INTERVAL__INSTANTIATE (Real);

