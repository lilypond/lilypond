/*
  interval.hh -- part of flowerlib

  (c) 1996--2006 Han-Wen Nienhuys
*/

#ifndef INTERVAL_HH
#define INTERVAL_HH

#include <math.h>

#include "std-string.hh"

#include "flower-proto.hh"
#include "drul-array.hh"

/* A T interval.  This represents the closed interval [left,right].
   No invariants.  T must be a totally ordered ring (with division, anyway ..)
   At instantiation, the function infinity () has to be defined explicitely. */
template<class T>
struct Interval_t : public Drul_array<T>
{
  Drul_array<T>::elem;
  Drul_array<T>::elem_ref;

  static T infinity ();
  static std::string T_to_string (T arg);
  T center () const;
  void translate (T t)
  {
    elem_ref (LEFT) += t;
    elem_ref (RIGHT) += t;
  }
  void widen (T t)
  {
    elem_ref (LEFT) -= t;
    elem_ref (RIGHT) += t;
  }

  T distance (T t) const
  {
    if (t > elem (RIGHT))
      return T (t - elem (RIGHT));
    else if (t < elem (LEFT))
      return T (elem (LEFT) - t);
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
    elem_ref (LEFT) = min (elem (LEFT), p);
    elem_ref (RIGHT) = max (elem (RIGHT), p);
  }
  T length () const;
  T delta () const;
  void set_empty ();
  void set_full ();

  /*
    TODO: strip hungarian suffix.
  */
  bool is_empty () const
  {
    return elem (LEFT) > elem (RIGHT);
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
    elem_ref (LEFT) += r;
    elem_ref (RIGHT) += r;
    return *this;
  }
  Interval_t<T> &operator *= (T r)
  {
    if (!is_empty ())
      {
	elem_ref (LEFT) *= r;
	elem_ref (RIGHT) *= r;
	if (r < T (0))
	  swap ();
      }
    return *this;
  }

  Real linear_combination (Real x) const
  {
    Drul_array<Real> da (elem (LEFT), elem (RIGHT));
    return ::linear_combination (da, x);
  }
  std::string to_string () const;

  bool contains (T r) const;
  void negate ()
  {
    T r = -elem (LEFT);
    T l = -elem (RIGHT);
    elem_ref (LEFT) = l;
    elem_ref (RIGHT) = r;
  }

  void swap ()
  {
    T t = elem (LEFT);
    elem_ref (LEFT) = elem (RIGHT);
    elem_ref (RIGHT) = t;
  }

  static int left_comparison (Interval_t<T> const &a, Interval_t<T> const &b)
  {
    return sign (a[LEFT] - b[RIGHT]);
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
  return (elem (LEFT) + elem (RIGHT)) / T (2);
}

typedef Interval_t<Real> Interval;
typedef Interval_t<int> Slice;	// weird name


#endif // INTERVAL_HH

