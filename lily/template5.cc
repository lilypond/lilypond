/*
  template5.cc -- instantiate Intervals

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "moment.hh"
#include "real.hh"
#include "interval.hh"

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


template<>
Moment
Interval_t<Moment>::infinity ()
{
  Moment infty;
  
  infty.main_part_.set_infinite (1);
  return infty;
}


template<>
string
Interval_t<Moment>::T_to_string (Moment a)
{
  return a.to_string ();
}

template INTERVAL__INSTANTIATE (Moment);

template<>
Real
Interval_t<Real>::linear_combination (Real x) const
{
  Drul_array<Real> da (at (LEFT), at (RIGHT));
  return ::linear_combination (da, x);
}
