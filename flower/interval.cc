/*
  interval.cc -- instantiate Interval_t<Real>

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "interval.hh"
#include "real.hh"
#include "interval.tcc"


template INTERVAL__INSTANTIATE(Real);

Real
Interval_t<Real>::infinity() 
{
  return HUGE_VAL;
}
