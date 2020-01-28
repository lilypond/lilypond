/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "grob.hh"
#include "libc-extension.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"

LY_DEFINE (ly_grob_staff_position, "ly:grob-staff-position", 1, 0, 0, (SCM sg),
           "Return the Y-position of @var{sg} relative to the staff.")
{
  LY_ASSERT_SMOB (Grob, sg, 1);
  Grob *g = unsmob<Grob> (sg);
  Real pos = Staff_symbol_referencer::get_position (g);

  if (fabs (rint (pos) - pos) < 1e-6) // ugh.
    return scm_from_int ((int)my_round (pos));
  else
    return scm_from_double (pos);
}

LY_DEFINE (ly_position_on_line_p, "ly:position-on-line?", 2, 0, 0,
           (SCM sg, SCM spos),
           "Return whether @var{spos} is on a line of the staff associated"
           " with the grob @var{sg} (even on an extender line).")
{
  LY_ASSERT_SMOB (Grob, sg, 1);
  LY_ASSERT_TYPE (scm_is_number, spos, 2);
  Grob *g = unsmob<Grob> (sg);
  Grob *st = Staff_symbol_referencer::get_staff_symbol (g);
  int pos = scm_to_int (spos);
  bool on_line = st ? Staff_symbol::on_line (g, pos) : false;
  return scm_from_bool (on_line);
}

LY_DEFINE (ly_staff_symbol_line_thickness, "ly:staff-symbol-line-thickness", 1,
           0, 0, (SCM grob),
           "Returns the current staff-line thickness in the staff"
           " associated with @var{grob}, expressed as a multiple of the"
           " current staff-space height.")
{
  LY_ASSERT_SMOB (Grob, grob, 1);
  Grob *g = unsmob<Grob> (grob);
  Real thickness = Staff_symbol_referencer::line_thickness (g);
  return scm_from_double (thickness);
}

LY_DEFINE (ly_staff_symbol_staff_space, "ly:staff-symbol-staff-space", 1, 0, 0,
           (SCM grob),
           "Returns the current staff-space height in the staff"
           " associated with @var{grob}, expressed as a multiple of the"
           " default height of a staff-space in the traditional"
           " five-line staff.")
{
  LY_ASSERT_SMOB (Grob, grob, 1);
  Grob *g = unsmob<Grob> (grob);
  Real staff_space = Staff_symbol_referencer::staff_space (g);
  return scm_from_double (staff_space);
}

LY_DEFINE (ly_staff_symbol_staff_radius, "ly:staff-symbol-staff-radius", 1, 0,
           0, (SCM grob),
           "Returns the radius of the staff associated with"
           " @var{grob}.")
{
  LY_ASSERT_SMOB (Grob, grob, 1);
  Grob *g = unsmob<Grob> (grob);
  Real staff_radius = Staff_symbol_referencer::staff_radius (g);
  return scm_from_double (staff_radius);
}
