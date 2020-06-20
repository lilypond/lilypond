/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2020 Han-Wen Nienhuys

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

#include "flower-proto.hh"
#include "std-string.hh"
#include "drul-array.hh"

#include <cmath>

/* A T interval.  This represents the closed interval [left,right].
   No invariants.  T must be a totally ordered ring (with division, anyway ..)
   At instantiation, the function infinity () has to be defined explicitly. */
template<class T>
struct Interval_t : private Drul_array<T>
{
  using Drul_array<T>::at;
  using Drul_array<T>::operator [];
  using Drul_array<T>::linear_combination;

  template <typename T2>
  Interval_t (Interval_t<T2> const &src) : Interval_t (src[LEFT], src[RIGHT])
  {
  }

  static T infinity ();
  T center () const;
  void translate (T t)
  {
    at (LEFT) += t;
    at (RIGHT) += t;
  }
  void widen (T t)
  {
    at (LEFT) -= t;
    at (RIGHT) += t;
  }

  T distance (T t) const
  {
    if (t > at (RIGHT))
      return T (t - at (RIGHT));
    else if (t < at (LEFT))
      return T (at (LEFT) - t);
    else
      return T (0);
  }
  /**
     PRE
     *this and h are comparable
     */
  void unite (Interval_t<T> h);
  void intersect (Interval_t<T> h);
  void add_point (T p)
  {
    at (LEFT) = std::min (at (LEFT), p);
    at (RIGHT) = std::max (at (RIGHT), p);
  }
  T length () const;

  void set_empty ();
  void set_full ();

  /* Unites h and this interval, but in such a way
   that h will lie in a particular direction from this
   interval, with a minimum amount of space in between.
   (That is, h will be translated before we unite, if
   that is necessary to prevent overlap. */
  template<class Padding>
  void
  unite_disjoint (Interval_t h, Padding padding, Direction d)
  {
    T dir = d;
    T translation = dir * (at (d) + dir * padding - h.at (-d));
    if (translation > T (0))
      h.translate (translation);
    unite (h);
  }

  template <class Padding>
  Interval_t
  union_disjoint (Interval_t h, Padding padding, Direction d) const
  {
    Interval_t iv = *this;
    iv.unite_disjoint (h, padding, d);
    return iv;
  }

  bool is_empty () const
  {
    return at (LEFT) > at (RIGHT);
  }
  Interval_t ()
  {
    set_empty ();
  }
  Interval_t (Drul_array<T> const &src)
    : Drul_array<T> (src)
  {
  }

  Interval_t (T m, T M) : Drul_array<T> (m, M)
  {
  }
  Interval_t<T> &operator -= (T r)
  {
    *this += -r;
    return *this;
  }

  Interval_t<T> &operator += (T r)
  {
    at (LEFT) += r;
    at (RIGHT) += r;
    return *this;
  }

  template <class Factor>
  Interval_t<T> &operator *= (Factor r)
  {
    if (!is_empty ())
      {
        at (LEFT) *= r;
        at (RIGHT) *= r;
        if (r < T (0))
          swap ();
      }
    return *this;
  }

  std::string to_string () const;

  bool contains (T r) const;
  void negate ()
  {
    T r = -at (LEFT);
    T l = -at (RIGHT);
    at (LEFT) = l;
    at (RIGHT) = r;
  }

  void swap ()
  {
    T t = at (LEFT);
    at (LEFT) = at (RIGHT);
    at (RIGHT) = t;
  }

  static bool left_less (Interval_t<T> const &a, Interval_t<T> const &b)
  {
    return a[LEFT] < b[LEFT];
  }
};

/**
   inclusion ordering. Crash if not  comparable.
*/
template<class T>
int Interval__compare (const Interval_t<T> &, Interval_t<T> const &);

/**
   Inclusion ordering.  return -2 if not comparable
*/
template<class T>
int
_Interval__compare (const Interval_t<T> &a, Interval_t<T> const &b);

/*
  INLINE
*/

#include "compare.hh"

TEMPLATE_INSTANTIATE_COMPARE (Interval_t<T> &, Interval__compare, template<class T>);

template<class T>
inline Interval_t<T>
intersection (Interval_t<T> a, Interval_t<T> const &b)
{
  a.intersect (b);
  return a;
}

template<class T>
inline
Interval_t<T> operator + (T a, Interval_t<T> i)
{
  i += a;
  return i;
}

template<class T>
inline
Interval_t<T> operator - (Interval_t<T> i, T a)
{
  i += -a;
  return i;
}

template<class T>
inline
Interval_t<T> operator - (T a, Interval_t<T> i)
{
  i.negate ();
  i += a;
  return i;
}

template<class T>
inline
Interval_t<T> operator + (Interval_t<T> i, T a)
{
  return a + i;
}

template<class T>
inline
Interval_t<T> operator * (T a, Interval_t<T> i)
{
  i *= a;
  return i;
}

template<class T>
inline
Interval_t<T> operator * (Interval_t<T> i, T a)
{
  return a * i;
}

template<class T>
inline T
Interval_t<T>::center () const
{
  assert (!is_empty ());
  return (at (LEFT) + at (RIGHT)) / 2;
}

typedef Interval_t<Real> Interval;
typedef Interval_t<int> Slice;  // weird name


#endif // INTERVAL_HH
