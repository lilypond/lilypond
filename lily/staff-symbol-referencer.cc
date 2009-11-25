/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "staff-symbol-referencer.hh"

#include "staff-symbol.hh"
#include "grob.hh"
#include "output-def.hh"
#include "libc-extension.hh"

int
Staff_symbol_referencer::line_count (Grob *me)
{
  Grob *st = get_staff_symbol (me);
  return st ? Staff_symbol::line_count (st) : 0;
}

bool
Staff_symbol_referencer::on_line (Grob *me, int pos)
{
  Grob *st = get_staff_symbol (me);
  return st ? Staff_symbol::on_line (st, pos) : false;
}

bool
Staff_symbol_referencer::on_staff_line (Grob *me, int pos)
{
  return on_line (me, pos) && abs (pos) <= 2 * staff_radius (me);
}

Grob *
Staff_symbol_referencer::get_staff_symbol (Grob *me)
{
  if (Staff_symbol::has_interface (me))
    return me;

  SCM st = me->get_object ("staff-symbol");
  return unsmob_grob (st);
}

Real
Staff_symbol_referencer::staff_space (Grob *me)
{
  Grob *st = get_staff_symbol (me);
  if (st)
    return Staff_symbol::staff_space (st);
  return 1.0;
}

Real
Staff_symbol_referencer::line_thickness (Grob *me)
{
  Grob *st = get_staff_symbol (me);
  if (st)
    return Staff_symbol::get_line_thickness (st);
  return me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
}

Real
Staff_symbol_referencer::get_position (Grob *me)
{
  Real p = 0.0;
  Grob *st = get_staff_symbol (me);
  Grob *c = st ? me->common_refpoint (st, Y_AXIS) : 0;
  if (st && c)
    {
      Real y = me->relative_coordinate (c, Y_AXIS)
	- st->relative_coordinate (c, Y_AXIS);

      p += 2.0 * y / Staff_symbol::staff_space (st);
      return p;
    }
  else if (!st)
    return me->relative_coordinate (me->get_parent (Y_AXIS), Y_AXIS) * 2;
  return robust_scm2double (me->get_property ("staff-position"), p);
}


Interval
Staff_symbol_referencer::extent_in_staff (Grob *me)
{
  Grob *st = get_staff_symbol (me);
  Grob *c = st ? me->common_refpoint (st, Y_AXIS) : 0;

  Interval retval;
  if (st && c)
    {
      retval  = me->extent (c, Y_AXIS)
	- st->relative_coordinate (c, Y_AXIS);
    }

  return retval;
}

int
Staff_symbol_referencer::get_rounded_position (Grob *me)
{
  return int (rint (get_position (me)));
}

MAKE_SCHEME_CALLBACK (Staff_symbol_referencer, callback, 1);
SCM
Staff_symbol_referencer::callback (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  SCM pos = me->get_property ("staff-position");
  Real off = 0.0;
  if (scm_is_number (pos))
    {
      Real space = Staff_symbol_referencer::staff_space (me);
      off = scm_to_double (pos) * space / 2.0;
    }

  return scm_from_double (off);
}

/*  This sets the position relative to the center of the staff symbol.

The function is hairy, because it can be called in two situations:

1. There is no staff yet; we must set staff-position

2. There is a staff, and perhaps someone even applied a
translate_axis (). Then we must compensate for the translation

In either case, we set a callback to be sure that our new position
will be extracted from staff-position */

void
Staff_symbol_referencer::set_position (Grob *me, Real p)
{
  Grob *st = get_staff_symbol (me);
  Real oldpos = 0.0;
  if (st && me->common_refpoint (st, Y_AXIS))
    {
      oldpos = get_position (me);
    }

  
  Real ss = Staff_symbol_referencer::staff_space (me);
  me->translate_axis ((p  - oldpos) * ss * 0.5, Y_AXIS);
}

/* Half of the height, in staff space, i.e. 2.0 for a normal staff. */
Real
Staff_symbol_referencer::staff_radius (Grob *me)
{
  return (line_count (me) - 1) / 2.0;
}

int
compare_position (Grob *const &a, Grob *const &b)
{
  return sign (Staff_symbol_referencer::get_position ((Grob *)a)
	       - Staff_symbol_referencer::get_position ((Grob *) b));
}

bool
position_less (Grob *const &a, Grob *const &b)
{
  return Staff_symbol_referencer::get_position (a)
    < Staff_symbol_referencer::get_position (b);
}

ADD_INTERFACE (Staff_symbol_referencer,
	       "An object whose Y@tie{}position is meant relative to a staff"
	       " symbol.  These usually"
	       " have @code{Staff_symbol_referencer::callback} in their"
	       " @code{Y-offset-callbacks}.",

	       /* properties */
	       "staff-position "
	       );
