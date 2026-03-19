/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef DRUL_ARRAY_HH
#define DRUL_ARRAY_HH

#include "direction.hh"
#include "real.hh"

#include <cassert>
#include <tuple>
#include <utility>

/**
   Left/right or Up/down arrays. Drul is nicer sounding than udlr
*/
template <class T>
struct Drul_array
{
private:
  T array_[2];

public:
  // By default, value-initialize both elements.
  constexpr Drul_array ()
    : array_ {}
  {
  }
  constexpr Drul_array (T const &t1, T const &t2)
    : array_ {t1, t2}
  {
  }
  constexpr Drul_array (const Drul_array &) = default;
  constexpr Drul_array (Drul_array &&) = default;
  ~Drul_array () = default; // N.B. non-virtual

  Drul_array &operator= (const Drul_array &) & = default;
  Drul_array &operator= (Drul_array &&) & = default;

  constexpr T &at (Direction d) &
  {
    assert (d);
    return array_[d > CENTER];
  }

  constexpr T const &at (Direction d) const &
  {
    assert (d);
    return array_[d > CENTER];
  }

  constexpr T &&at (Direction d) &&
  {
    assert (d);
    return std::move (array_[d > CENTER]);
  }

  constexpr T const &&at (Direction d) const &&
  {
    assert (d);
    return std::move (array_[d > CENTER]);
  }

  // The operator[] in std classes often doesn't check bounds, but we consider
  // it prudent; therefore, we call at().
  constexpr T &operator[] (Direction d) & { return at (d); }
  constexpr T const &operator[] (Direction d) const & { return at (d); }
  constexpr T &&operator[] (Direction d) && { return std::move (at (d)); }
  constexpr T const &&operator[] (Direction d) const &&
  {
    return std::move (at (d));
  }

  // at (Direction::negative ())
  constexpr T &front () & { return array_[0]; }
  constexpr T const &front () const & { return array_[0]; }
  constexpr T &&front () && { return std::move (array_[0]); }
  constexpr T const &&front () const && { return std::move (array_[0]); }

  // at (Direction::positive ())
  constexpr T &back () & { return array_[1]; }
  constexpr T const &back () const & { return array_[1]; }
  constexpr T &&back () && { return std::move (array_[1]); }
  constexpr T const &&back () const && { return std::move (array_[1]); }

  template <std::size_t index, class U>
  static constexpr decltype (auto) internal_drul_array_get (U &&arr)
  {
    static_assert (index < 2);
    return std::forward<U> (arr).array_[index];
  }
};

namespace std // specializations for a tuple-like interface
{

template <class T>
struct tuple_size<::Drul_array<T>>
{
  static constexpr size_t value = 2;
};

template <size_t Index, class T>
struct tuple_element<Index, ::Drul_array<T>>
  : tuple_element<Index, tuple<T, T>>
{
};

} // namespace std

// a tuple-like interface supports structured binding
template <std::size_t index, class T>
constexpr auto
get (T &&arr) -> decltype (arr.template internal_drul_array_get<index> (
  std::forward<T> (arr)))
{
  // The trailing return type provides SFINAE: this get() matches when T has an
  // internal_drul_array_get() function, which should only be Drul_array.
  //
  // Passing the array as a seemingly redundant function argument lets us
  // define the function once with a forwarding reference instead of defining
  // overrides for T&, const T&, T&&, and const T&&.
  return arr.template internal_drul_array_get<index> (std::forward<T> (arr));
}

template <class T1, class T2>
constexpr void
scale_drul (Drul_array<T1> *dr, T2 x)
{
  dr->front () *= x;
  dr->back () *= x;
}

#endif /* DRUL_ARRAY_HH */
