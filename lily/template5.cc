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

Interval__instantiate(Rational);
Interval__instantiate(int);

Rational
Interval_t<Rational>::infinity()
{
    return Rational(INT_MAX);
}

int
Interval_t<int>::infinity()
{
    return INT_MAX;
}
