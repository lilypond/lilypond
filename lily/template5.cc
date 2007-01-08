/*
  template5.cc -- instantiate Intervals

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "moment.hh"

#include "interval.tcc"

template<>
Rational
Interval_t<Rational>::infinity ()
{
  Rational infty;
  infty.set_infinite (1);
  return infty;
}

template<>
string
Interval_t<Rational>::T_to_string (Rational a)
{
  return a.to_string ();
}

template INTERVAL__INSTANTIATE (Rational);
