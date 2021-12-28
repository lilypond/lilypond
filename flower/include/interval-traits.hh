/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#ifndef INTERVAL_TRAITS_HH
#define INTERVAL_TRAITS_HH

#include "flower-proto.hh"

#include <limits>

// This tells Interval_t<T> the bounds of the longest interval.  The default
// implementation supports any negatable type having an infinity () method.
// Other types can be supported by specializing this template.
template <class T>
struct Interval_traits
{
  static constexpr T max () { return T::infinity (); }
  static constexpr T min () { return -T::infinity (); }
};

template <>
struct Interval_traits<double>
{
  using limits = std::numeric_limits<double>;
  static constexpr double max () { return limits::infinity (); }
  static constexpr double min () { return -limits::infinity (); }
};

template <>
struct Interval_traits<int>
{
  using limits = std::numeric_limits<int>;
  static constexpr int max () { return limits::max (); }
  // -max () follows the previous implementation to reduce the risk of breaking
  // something.  Consider changing it to min () at some point.
  static constexpr int min () { return -limits::max (); }
};

// N.B. Interval_t was designed for signed values and later used for vsize.
// There might be traps waiting to be sprung.
template <>
struct Interval_traits<vsize>
{
  using limits = std::numeric_limits<vsize>;
  static constexpr vsize max () { return limits::max (); }
  static constexpr vsize min () { return limits::min (); }
};

#endif // INTERVAL_TRAITS_HH
