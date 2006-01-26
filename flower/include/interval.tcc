/*
  interval.tcc -- implement Interval_t

  source file of the Flower Library

  (c) 1996--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  if (a.elem (LEFT) == b.elem (LEFT) && a.elem (RIGHT) == b.elem (RIGHT))
    return 0;

  if (a.elem (LEFT) <= b.elem (LEFT) && a.elem (RIGHT) >= b.elem (RIGHT))
    return 1;

  if (a.elem (LEFT) >= b.elem (LEFT) && a.elem (RIGHT) <= b.elem (RIGHT))
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
  elem_ref (LEFT) = (T) infinity ();
  elem_ref (RIGHT) = (T) -infinity ();
}

template<class T>
void
Interval_t<T>::set_full ()
{
  elem_ref (LEFT) = (T) -infinity ();
  elem_ref (RIGHT) = (T) infinity ();
}

template<class T>
T
Interval_t<T>::length () const
{
  if (elem (RIGHT) <= elem (LEFT))
    return 0;
  else
    return elem (RIGHT) - elem (LEFT);
}

template<class T>
T
Interval_t<T>::delta () const
{
  return elem (RIGHT) - elem (LEFT);
}

/* smallest Interval which includes *this and #h#  */
template<class T>
void
Interval_t<T>::unite (Interval_t<T> h)
{
  elem_ref (LEFT) = min (h.elem (LEFT), elem (LEFT));
  elem_ref (RIGHT) = max (h.elem (RIGHT), elem (RIGHT));
}

template<class T>
void
Interval_t<T>::intersect (Interval_t<T> h)
{
  elem_ref (LEFT) = max (h.elem (LEFT), elem (LEFT));
  elem_ref (RIGHT) = min (h.elem (RIGHT), elem (RIGHT));
}

template<class T>
std::string
Interval_t<T>::to_string () const
{
  if (is_empty ())
    return "[empty]";
  std::string s ("[");

  return (s + T_to_string (elem (LEFT)) + std::string (",")
	  + T_to_string (elem (RIGHT)) + std::string ("]"));
}

template<class T>
bool
Interval_t<T>::contains (T r) const
{
  return r >= elem (LEFT) && r <= elem (RIGHT);
}

#define INTERVAL__INSTANTIATE(T) struct Interval_t<T>;			\
  template int Interval__compare (const Interval_t<T> &, Interval_t<T> const &)

#endif // INTERVAL_TCC
