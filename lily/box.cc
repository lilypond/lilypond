/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "box.hh"

void
Box::translate (Offset o)
{
  for (const auto a : {X_AXIS, Y_AXIS})
    if (!is_empty (a))
      interval_a_[a] += o[a];
}

void
Box::unite (Box b)
{
  for (const auto a : {X_AXIS, Y_AXIS})
    interval_a_[a].unite (b[a]);
}

Real
Box::area () const
{
  return interval_a_[X_AXIS].length () * interval_a_[Y_AXIS].length ();
}

Box::Box ()
{
}

void
Box::set_empty ()
{
  interval_a_[X_AXIS].set_empty ();
  interval_a_[Y_AXIS].set_empty ();
}

bool
Box::is_empty () const
{
  return is_empty (X_AXIS) && is_empty (Y_AXIS);
}

bool
Box::is_empty (Axis a) const
{
  Interval empty;
  empty.set_empty ();
  return interval_a_[a][LEFT] == empty[LEFT]
         && interval_a_[a][RIGHT] == empty[RIGHT];
}

Box::Box (Interval ix, Interval iy)
{
  x () = ix;
  y () = iy;
}

Interval &
Box::operator[] (Axis a)
{
  return interval_a_[a];
}

Interval
Box::operator[] (Axis a) const
{
  return interval_a_[a];
}

void
Box::scale (Real s)
{
  interval_a_[X_AXIS] *= s;
  interval_a_[Y_AXIS] *= s;
}

void
Box::add_point (Offset o)
{
  interval_a_[X_AXIS].add_point (o[X_AXIS]);
  interval_a_[Y_AXIS].add_point (o[Y_AXIS]);
}

Offset
Box::center () const
{
  return Offset (interval_a_[X_AXIS].center (), interval_a_[Y_AXIS].center ());
}

void
Box::widen (Real x, Real y)
{
  interval_a_[X_AXIS].widen (x);
  interval_a_[Y_AXIS].widen (y);
}

void
Box::intersect (Box b)
{
  interval_a_[X_AXIS].intersect (b[X_AXIS]);
  interval_a_[Y_AXIS].intersect (b[Y_AXIS]);
}

// for debugging

void
Box::print ()
{
  printf ("X left %4.4f right %4.4f Y down %4.4f up %4.4f\n",
          interval_a_[X_AXIS][LEFT], interval_a_[X_AXIS][RIGHT],
          interval_a_[Y_AXIS][DOWN], interval_a_[Y_AXIS][UP]);
}

/****************************************************************/

const char *const Box::type_p_name_ = "ly:box?";
