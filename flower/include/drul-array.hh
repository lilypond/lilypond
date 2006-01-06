/*
  drul-array.hh -- declare Drul_array

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef DRUL_ARRAY_HH
#define DRUL_ARRAY_HH

#include "direction.hh"
#include "real.hh"

/**
   Left/right or Up/down arrays. Drul is nicer sounding than udlr
*/
template<class T>
struct Drul_array
{
  T array_[2];
  T &elem_ref (Direction d)
  {
    assert (d == 1 || d == -1);
    return array_[ (d + 1) / 2];
  }
  T elem (Direction d) const
  {
    assert (d == 1 || d == -1);
    return array_[ (d + 1) / 2];
  }
  T &operator [] (Direction d)
  {
    return elem_ref (d);
  }
  T operator [] (Direction d) const
  {
    return elem (d);
  }
  Drul_array ()
  {
  }
  Drul_array (T t1, T t2)
  {
    array_[0] = t1;
    array_[1] = t2;
  }
};

template<class T>
void
scale_drul (Drul_array<T> *dr, T x)
{
  dr->elem_ref (LEFT) *= x;
  dr->elem_ref (RIGHT) *= x;
}

inline Real
linear_combination (Drul_array<Real> const &d, Real x)
{
  return ((1.0 - x) * Real (d.elem (LEFT))
	  + (x + 1.0) * Real (d.elem (RIGHT))) * 0.5;
}

#endif /* DRUL_ARRAY_HH */
