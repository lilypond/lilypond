/*
  template5.cc -- instantiate Intervals

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <limits.h>
#include "proto.hh"
#include "string.hh"
#include "moment.hh"
#include "real.hh"

#include "interval.tcc"

template INTERVAL__INSTANTIATE(Rational);
template INTERVAL__INSTANTIATE(int);

#include "compare.hh"

Rational
Interval_t<Rational>::infinity()
{
  Rational infty;
  infty.set_infinite (1);
  return infty;
}

String
Interval_t<Rational>::T_to_str (Rational a)
{
  return a.str ();
}

int
Interval_t<int>::infinity()
{
  return INT_MAX;
}

String
Interval_t<int>::T_to_str (int i)
{
  return String (i);
}
