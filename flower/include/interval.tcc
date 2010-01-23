/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef INTERVAL_TCC
#define INTERVAL_TCC

#include <cassert>

#include "interval.hh"
#include "std-string.hh"

// MacOS 10.3 problems:
// #include <cmath>
using namespace std;

template<class T>
int
_Interval__compare (const Interval_t<T> &a, Interval_t<T> const &b)
{
  if (a.at (LEFT) == b.at (LEFT) && a.at (RIGHT) == b.at (RIGHT))
    return 0;

  if (a.at (LEFT) <= b.at (LEFT) && a.at (RIGHT) >= b.at (RIGHT))
    return 1;

  if (a.at (LEFT) >= b.at (LEFT) && a.at (RIGHT) <= b.at (RIGHT))
    return -1;

  return -2;
}

template<class T>
bool
Interval_t<T>::superset (Interval_t<T> const &a) const
{
  int c_i = _Interval__compare (*this, a);
  if (c_i == -2)
    return false;
  return c_i >= 0;
}

template<class T>
int
Interval__compare (Interval_t<T> const &a, Interval_t<T> const &b)
{
  int i = _Interval__compare (a, b);
  if (i < -1)
    assert (false);
  return i;
}

template<class T>
void
Interval_t<T>::set_empty ()
{
  at (LEFT) = (T) infinity ();
  at (RIGHT) = (T) -infinity ();
}

template<class T>
void
Interval_t<T>::set_full ()
{
  at (LEFT) = (T) -infinity ();
  at (RIGHT) = (T) infinity ();
}

template<class T>
T
Interval_t<T>::length () const
{
  if (at (RIGHT) <= at (LEFT))
    return 0;
  else
    return at (RIGHT) - at (LEFT);
}

template<class T>
T
Interval_t<T>::delta () const
{
  return at (RIGHT) - at (LEFT);
}

/* smallest Interval which includes *this and #h#  */
template<class T>
void
Interval_t<T>::unite (Interval_t<T> h)
{
  at (LEFT) = min (h.at (LEFT), at (LEFT));
  at (RIGHT) = max (h.at (RIGHT), at (RIGHT));
}

template<class T>
void
Interval_t<T>::intersect (Interval_t<T> h)
{
  at (LEFT) = max (h.at (LEFT), at (LEFT));
  at (RIGHT) = min (h.at (RIGHT), at (RIGHT));
}

template<class T>
string
Interval_t<T>::to_string () const
{
  if (is_empty ())
    return "[empty]";
  string s ("[");

  return (s + T_to_string (at (LEFT)) + string (",")
	  + T_to_string (at (RIGHT)) + string ("]"));
}

template<class T>
bool
Interval_t<T>::contains (T r) const
{
  return r >= at (LEFT) && r <= at (RIGHT);
}

#define INTERVAL__INSTANTIATE(T) struct Interval_t<T>;			\
  template int Interval__compare (const Interval_t<T> &, Interval_t<T> const &)

#endif // INTERVAL_TCC
