/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
