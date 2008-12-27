/*
  box.cc -- implement Box

  source file of the GNU LilyPond music typesetter

  (c) 1996--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
