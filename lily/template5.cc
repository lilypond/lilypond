/*
  template5.cc -- instantiate Intervals

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <limits.h>
#include "string.hh"
#include "moment.hh"
#include "real.hh"
#include "interval.tcc"
#include "compare.hh"

Rational
Interval_t<Rational>::infinity ()
{
  Rational infty;
  infty.set_infinite (1);
  return infty;
}

String
Interval_t<Rational>::T_to_string (Rational a)
{
  return a.string ();
}



template INTERVAL__INSTANTIATE (Rational);
