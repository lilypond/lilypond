/*
  drul-array.hh -- declare Drul_array

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DRUL_ARRAY_HH
#define DRUL_ARRAY_HH

#include "direction.hh"

#include <assert.h>

/**
  Left/right or Up/down arrays. Drul is nicer sounding than udlr
 */
template<class T>
struct Drul_array
{
  T array_[2];
  T &operator[] (Direction d)
  {
    assert (d==1 || d== -1);
    return array_[(d+1)/2];
  }
  T operator[]  (Direction d) const
  {
    assert (d==1 || d== -1);
    return array_[(d+1)/2];
  }
};

#endif // DRUL_ARRAY_HH
