/*
  interval.hh -- part of flowerlib

  (c) 1996--2008 Han-Wen Nienhuys
*/

#ifndef INTERVAL_HH
#define INTERVAL_HH

#include <math.h>

#include "flower-proto.hh"
#include "drul-array.hh"

/* A T interval.  This represents the closed interval [left,right].
   No invariants.  T must be a totally ordered ring (with division, anyway ..)
   At instantiation, the function infinity () has to be defined explicitely. */
template<class T>
struct Interval_t : public Drul_array<T>
{
  Drul_array<T>::at;

  static T infinity ();
  static string T_to_string (T arg);
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
    at (LEFT) = min (at (LEFT), p);
    at (RIGHT) = max (at (RIGHT), p);
  }
  T length () const;
  T delta () const;
  void set_empty ();
  void set_full ();

  bool is_empty () const
  {
    return at (LEFT) > at (RIGHT);
  }
  bool superset (Interval_t<T> const &) const;
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
  Interval_t<T> &operator *= (T r)
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

  Real linear_combination (Real x) const;
  string to_string () const;

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
    return a[LEFT] < b[RIGHT];
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
  return (at (LEFT) + at (RIGHT)) / T (2);
}

typedef Interval_t<Real> Interval;
typedef Interval_t<int> Slice;	// weird name


#endif // INTERVAL_HH

