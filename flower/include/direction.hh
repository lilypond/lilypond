/*
  direction.hh -- declare Direction

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef DIRECTION_HH
#define DIRECTION_HH

#include "axis.hh"

enum Direction
  {
    UP = 1,
    DOWN=-1,
    LEFT=-1,
    RIGHT = 1,
    MIN=-1,
    MAX = 1,
    CENTER = 0,
    SMALLER=-1,
    BIGGER = 1,
    START = -1,
    STOP = 1,

    /*
      This is necessary to safely write loops,
      since

      dir <= RIGHT

      is otherwise transformed into true unconditionally.
    */
    DIRECTION_LIMIT = 2,
    DIRECTION_NEG_LIMIT = -2,
  };

inline Direction
other_dir (Direction const d)
{
  return (Direction) (-d);
}

inline Direction
operator - (Direction const d)
{
  return other_dir (d);
}

// huh?
inline Direction
flip (Direction *i)
{
  if (*i == (Direction)1)
    *i = (Direction) - 1;
  else if (*i == (Direction) - 1)
    *i = (Direction)1;
  return *i;
}

/**
   if d > 0: the max operator
   if d < 0: the min operator
*/
template<class T> T minmax (Direction d, T, T);

// String direction_string (Direction, Axis);

#endif // DIRECTION_HH
