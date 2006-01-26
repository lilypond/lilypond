/*
  interval.cc -- instantiate Interval_t<Real>

  source file of the Flower Library

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "interval.hh"

#include "interval.tcc"

template<>
Real
Interval_t<Real>::infinity ()
{
  return HUGE_VAL;
}

template<>
std::string
Interval_t<Real>::T_to_string (Real r)
{
  return std::to_string (r);
}

template<>
int
Interval_t<int>::infinity ()
{
  return INT_MAX;
}

template<>
std::string
Interval_t<int>::T_to_string (int i)
{
  return std::to_string (i);
}

template INTERVAL__INSTANTIATE (int);
template INTERVAL__INSTANTIATE (Real);
