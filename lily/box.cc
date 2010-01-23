/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  for (Axis i = X_AXIS; i < NO_AXES; incr (i))
    interval_a_[i] += o[i];
}

void
Box::unite (Box b)
{
  for (Axis i = X_AXIS; i < NO_AXES; incr (i))
    interval_a_[i].unite (b[i]);
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

Box::Box (Interval ix, Interval iy)
{
  x () = ix;
  y () = iy;
}

Interval &
Box::operator [] (Axis a)
{
  return interval_a_[a];
}

Interval
Box::operator [] (Axis a) const
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
  return Offset (interval_a_[X_AXIS].center (),
		 interval_a_[Y_AXIS].center ());
}

void
Box::widen (Real x, Real y)
{
  interval_a_[X_AXIS].widen (x);
  interval_a_[Y_AXIS].widen (y);
}

/****************************************************************/

#include "ly-smobs.icc"

IMPLEMENT_SIMPLE_SMOBS (Box);
IMPLEMENT_TYPE_P (Box, "ly:box?");
IMPLEMENT_DEFAULT_EQUAL_P (Box);

SCM
Box::mark_smob (SCM /* x */)
{
  return SCM_EOL;
}

int
Box::print_smob (SCM /* x */, SCM p, scm_print_state*)
{
  scm_puts ("#<Box>", p);
  return 1;
}
