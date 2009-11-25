/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef REAL_HH
#define REAL_HH

#include <algorithm>
#include <climits>
#include <cmath>
using namespace std;

typedef double Real;
extern const Real infinity_f;

/* namespace std { */
  
template<class T> inline T abs (T x)
{
  return x > 0 ? x : -x;
}

/* } namespace std */

inline Real
distance (Real x, Real y)
{
  return abs (x - y);
}

template<class T> inline int sign (T x)
{
  if (x)
    return x > T (0) ? 1 : -1;
  return 0;
}

template<class T> inline T sqr (T x)
{
  return x * x;
}

#endif
