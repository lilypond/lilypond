/*
  axes.hh -- declare Axis

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef AXES_HH
#define AXES_HH

#include <cassert>
using namespace std;

enum Axis
  {
    X_AXIS = 0,
    Y_AXIS = 1,
    NO_AXES = 2,
  };

static inline
Axis
incr (Axis &a)
{
  assert (a < NO_AXES);
  a = Axis (int (a) + 1);
  return a;
}

static inline
Axis
other_axis (Axis a)
{
  return a == Y_AXIS ? X_AXIS : Y_AXIS;
}

#endif // AXES_HH
