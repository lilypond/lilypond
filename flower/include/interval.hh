/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys

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

#ifndef INTERVAL_HH
#define INTERVAL_HH

// Needed because of extension definitions for POSIX functions.
#include "config.hh"

#include "interval-traits.hh"
#include "std-string.hh"
#include "drul-array.hh"

#include <algorithm>

// A T interval.  This represents the closed interval [left,right].
// No invariants.  T must be a totally ordered ring (with division, anyway ..)
template <class T>
struct Interval_t : private Drul_array<T>
{
private:
  using base_type = Drul_array<T>;
  using difference_type = decltype (T () - T ());

public:
  using base_type::at;
  using base_type::operator[];

  // empty interval
  constexpr Interval_t ()
    : base_type (Interval_traits<T>::max (), Interval_traits<T>::min ())
  {
  }

  // degenerate interval (a point, if the argument is finite)
  constexpr explicit Interval_t (T m)
    : Interval_t (m, m)
  {
  }

  constexpr Interval_t (T m, T M)
    : base_type (m, M)
  {
  }

  // Allow implicit conversion from another kind of Interval_t if its element
  // type is convertible to this one's.
  template <typename T2>
  constexpr Interval_t (Interval_t<T2> const &src)
    : base_type (src.left (), src.right ())
  {
  }

  // interval of maximum extent
  static constexpr Interval_t longest ()
  {
    // The name full () was avoided because it sounds like a predicate next to
    // STL empty ().
    return {Interval_traits<T>::min (), Interval_traits<T>::max ()};
  }

  T center () const
  {
    assert (!is_empty ());
    return unchecked_center ();
  }

  // If t lies outside this interval to the left or the right, return left ()
  // or right () respectively; otherwise, return a reference to t.
  constexpr T const &clamp (T const &t) const
  {
    if (!is_empty ())
      {
        if (t < left ())
          return left ();

        if (t > right ())
          return right ();
      }

    return t;
  }

  T &left () { return base_type::front (); }

  constexpr T const &left () const { return base_type::front (); }

  // Given x in [-1.0, 1.0], interpolate linearly between the left and right
  // bounds of this Interval.  For other values, extrapolate linearly.
  T linear_combination (Real x) const;

  // Find the position of the given point relative to the center of the
  // interval in terms of the length of the interval: -1 at the left endpoint
  // and +1 at the right.  This is the inverse of linear_combination ().
  Real inverse_linear_combination (const T &t) const;

  T &right () { return base_type::back (); }

  constexpr T const &right () const { return base_type::back (); }

  template <typename T2>
  Interval_t<T> &translate (const T2 &t)
  {
    return *this += t;
  }

  template <typename T2>
  void widen (const T2 &t)
  {
    left () -= t;
    right () += t;
  }

  difference_type distance (T t) const
  {
    if (t > right ())
      return t - right ();
    else if (t < left ())
      return left () - t;
    else
      return difference_type (0);
  }

  // Lengthen this Interval by the minimum amount necessary to include all
  // points in h.
  void unite (Interval_t<T> h)
  {
    left () = std::min (h.left (), left ());
    right () = std::max (h.right (), right ());
  }

  // Shorten this Interval by the minimum amount necessary to exclude all
  // points that are not in h; in other words, keep what overlaps with h.
  void intersect (Interval_t<T> h)
  {
    left () = std::max (h.left (), left ());
    right () = std::min (h.right (), right ());
  }

  // Lengthen this Interval by the minimum amount necessary to include p.
  void add_point (T p)
  {
    left () = std::min (left (), p);
    right () = std::max (right (), p);
  }

  // Notes:
  // * The length of an interval with a NaN bound is NaN.
  // * A point interval has a length of zero but is not empty.
  // * (-infinity, -infinity) has undefined length but is nonempty.
  // * (infinity, infinity) has undefined length but is nonempty.
  constexpr difference_type length () const
  {
    return !is_empty () ? (right () - left ()) : difference_type (0);
  }

  void set_empty ()
  {
    left () = Interval_traits<T>::max ();
    right () = Interval_traits<T>::min ();
  }

  void set_full ()
  {
    left () = Interval_traits<T>::min ();
    right () = Interval_traits<T>::max ();
  }

  /* Unites h and this interval, but in such a way
   that h will lie in a particular direction from this
   interval, with a minimum amount of space in between.
   (That is, h will be translated before we unite, if
   that is necessary to prevent overlap. */
  template <class Padding>
  void unite_disjoint (Interval_t h, Padding padding, Direction d)
  {
    const auto translation (d * (at (d) + d * padding - h.at (-d)));
    if (translation > decltype (translation) (0))
      h.translate (translation);
    unite (h);
  }

  template <class Padding>
  Interval_t union_disjoint (Interval_t h, Padding padding, Direction d) const
  {
    Interval_t iv = *this;
    iv.unite_disjoint (h, padding, d);
    return iv;
  }

  // Notes:
  // * An interval with a NaN bound is considered nonempty.
  // * A point interval has a length of zero but is not empty.
  // * (-infinity, -infinity) has undefined length but is nonempty.
  // * (infinity, infinity) has undefined length but is nonempty.
  constexpr bool is_empty () const { return left () > right (); }

  template <typename T2>
  Interval_t<T> &operator-= (T2 r)
  {
    *this += -r;
    return *this;
  }

  template <typename T2>
  Interval_t<T> &operator+= (const T2 &r)
  {
    left () += r;
    right () += r;
    return *this;
  }

  template <class Factor>
  Interval_t<T> &operator*= (Factor r)
  {
    if (!is_empty ())
      {
        left () *= r;
        right () *= r;
        if (r < T (0))
          swap ();
      }
    return *this;
  }

  std::string to_string () const;

  constexpr bool contains (T r) const { return r >= left () && r <= right (); }

  void negate ()
  {
    T r = -left ();
    T l = -right ();
    left () = l;
    right () = r;
  }

  void swap ()
  {
    T t = left ();
    left () = right ();
    right () = t;
  }

  static constexpr bool left_less (Interval_t<T> const &a,
                                   Interval_t<T> const &b)
  {
    return a.left () < b.left ();
  }

private:
  constexpr T unchecked_center () const
  {
    // Subtracting T() allows averaging types that support subtraction but not
    // addition, e.g. std::chrono::time_point<>.
    return T (((left () - T ()) + (right () - T ())) / 2);
  }
};

/**
   inclusion ordering. Crash if not  comparable.
*/
template <class T>
int Interval__compare (const Interval_t<T> &, Interval_t<T> const &);

/**
   Inclusion ordering.  return -2 if not comparable
*/
template <class T>
int _Interval__compare (const Interval_t<T> &a, Interval_t<T> const &b);

/*
  INLINE
*/

#include "compare.hh"

TEMPLATE_INSTANTIATE_COMPARE (Interval_t<T> &, Interval__compare,
                              template <class T>
);

template <>
inline Real
Interval_t<Real>::center () const
{
  // This specialization omits the assert () that is present in the default
  // implementation so that iv.center () can be used in the same circumstances
  // as iv.linear_combination (0).  A Real result can represent infinity or
  // NaN, so there is no need to crash on empty, unbounded, or reversed
  // intervals.
  return unchecked_center ();
}

template <>
inline Real
Interval_t<Real>::linear_combination (Real x) const
{
  return (((1.0 - x) * left ()) + ((x + 1.0) * right ())) * 0.5;
}

template <>
inline Real
Interval_t<Real>::inverse_linear_combination (const Real &x) const
{
  return (x - center ()) / (length () * 0.5);
}

template <class T>
inline Interval_t<T>
intersection (Interval_t<T> a, Interval_t<T> const &b)
{
  a.intersect (b);
  return a;
}

template <class T>
inline Interval_t<T>
operator+ (T a, Interval_t<T> i)
{
  i += a;
  return i;
}

template <class T, class T2>
inline Interval_t<T>
operator- (Interval_t<T> i, T2 a)
{
  i += -a;
  return i;
}

template <class T>
inline Interval_t<T>
operator- (T a, Interval_t<T> i)
{
  i.negate ();
  i += a;
  return i;
}

template <class T, class T2>
inline Interval_t<T>
operator+ (Interval_t<T> i, T2 a)
{
  return i += a;
}

template <class T>
inline Interval_t<T>
operator* (T a, Interval_t<T> i)
{
  i *= a;
  return i;
}

template <class T>
inline Interval_t<T>
operator* (Interval_t<T> i, T a)
{
  return a * i;
}

template <class T>
inline std::string
to_string (Interval_t<T> const &i)
{
  return i.to_string ();
}

typedef Interval_t<Real> Interval;
typedef Interval_t<int> Slice; // weird name

#endif // INTERVAL_HH
