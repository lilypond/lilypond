/*
  interval.tcc -- implement Interval_t

  source file of the Flower Library

  (c) 1996, 1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef INTERVAL_TCC
#define INTERVAL_TCC

#include <assert.h> 
#include <math.h>
#include "interval.hh"
#include "string.hh"
#include "flower-debug.hh"

template<class T>
void
Interval_t<T>::print () const
{
  if (flower_dstream)
    *flower_dstream << str ();
}

template<class T>
int
_Interval__compare (const Interval_t<T>&a,Interval_t<T> const&b)
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
Interval_t<T>::contains_b (Interval_t<T> const& a) const
{
  int c_i= _Interval__compare (*this, a);
  if (c_i == -2)
    return false;
  return c_i >= 0;
}

template<class T>
int
Interval__compare (const Interval_t<T>&a,Interval_t<T> const&b)
{
  int i = _Interval__compare (a,b);
  if (i < -1)
    assert (false);
  return i;
}

template<class T>
void
Interval_t<T>::set_empty ()
{
  elem (LEFT) = (T) infinity ();
  elem (RIGHT) = (T) -infinity ();
}

template<class T>
T
Interval_t<T>::length () const 
{
  if (elem (RIGHT) < elem (LEFT)) 
    return 0;
  else 
    return elem (RIGHT)-elem (LEFT);
}

/**
  smallest Interval which includes *this and #h#
 */
template<class T>
void
Interval_t<T>::unite (Interval_t<T> h)
{
  elem (LEFT) = h.elem (LEFT) <? elem (LEFT);
  elem (RIGHT) = h.elem (RIGHT) >?elem (RIGHT);

#if 0
  if (h.elem (LEFT)<elem (LEFT))
    elem (LEFT) = h.elem (LEFT);
  if (h.elem (RIGHT)>elem (RIGHT))
  elem (RIGHT) = h.elem (RIGHT);
#endif
}


template<class T>
void
Interval_t<T>::intersect (Interval_t<T> h)
{
#if defined (__GNUG__) && ! defined (__STRICT_ANSI__)
  elem (LEFT) = h.elem (LEFT) >? elem (LEFT);
  elem (RIGHT) = h.elem (RIGHT) <?elem (RIGHT);
#else
  elem (LEFT) = max (h.elem (LEFT), elem (LEFT));
  elem (RIGHT) = min (h.elem (RIGHT), elem (RIGHT));
#endif
}

template<class T>
Interval_t<T>
intersect (Interval_t<T> x, Interval_t<T> const &y)
{
  x.intersect (y);
  return x;
}

template<class T>
String
Interval_t<T>::str () const
{
  if (empty_b ())
    return "[empty]";
  String s ("[");
 
  return s + T_to_str (elem (LEFT)) + String ("," ) + T_to_str (elem (RIGHT) ) + String ("]" );
}

template<class T>
bool
Interval_t<T>::elem_b (T r)
{
  return r >= elem (LEFT) && r <= elem (RIGHT);
}


#define INTERVAL__INSTANTIATE(T) struct Interval_t<T>;\
template  int Interval__compare (const Interval_t<T>&,Interval_t<T> const&)

#endif // INTERVAL_TCC
