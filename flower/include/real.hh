/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

// Needed because of extension definitions for POSIX functions.
#include "config.hh"

#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdlib>

typedef double Real;
static constexpr Real infinity_f = INFINITY;

using std::abs; // TODO: replace abs (x) with std::abs (x) and remove this

template <class T>
inline int
sign (T x)
{
  if (x != T (0))
    return std::signbit (x) ? -1 : 1;
  return 0;
}

template <class T>
constexpr auto
sqr (T x)
{
  return x * x;
}

#endif
