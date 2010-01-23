/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
