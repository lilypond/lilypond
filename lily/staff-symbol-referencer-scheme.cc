/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "config.hh"

#include "grob.hh"
#include "libc-extension.hh"
#include "staff-symbol.hh"
#include "staff-symbol-referencer.hh"

#include <cmath>

LY_DEFINE (ly_grob_staff_position, "ly:grob-staff-position", 1, 0, 0, (SCM sg),
           R"(
Return the y@tie{}position of @var{sg} relative to the staff.
           )")
{
  auto *const g = LY_ASSERT_SMOB (Grob, sg, 1);
  Real pos = Staff_symbol_referencer::get_position (g);

  if (fabs (rint (pos) - pos) < 1e-6) // ugh.
    return to_scm (static_cast<int> (round_halfway_up (pos)));
  else
    return to_scm (pos);
}

LY_DEFINE (ly_position_on_line_p, "ly:position-on-line?", 2, 0, 0,
           (SCM sg, SCM spos),
           R"(
Return whether @var{spos} is on a line of the staff associated with the grob
@var{sg} (even on an extender line).
           )")
{
  auto *const g = LY_ASSERT_SMOB (Grob, sg, 1);
  LY_ASSERT_TYPE (scm_is_number, spos, 2);
  int pos = from_scm<int> (spos);
  return to_scm (Staff_symbol_referencer::on_line (g, pos));
}

LY_DEFINE (ly_staff_symbol_line_thickness, "ly:staff-symbol-line-thickness", 1,
           0, 0, (SCM grob),
           R"(
Return the current staff line thickness in the staff associated with
@var{grob}, expressed as a multiple of the current staff space height.
           )")
{
  auto *const g = LY_ASSERT_SMOB (Grob, grob, 1);
  Real thickness = Staff_symbol_referencer::line_thickness (g);
  return to_scm (thickness);
}

LY_DEFINE (ly_staff_symbol_staff_space, "ly:staff-symbol-staff-space", 1, 0, 0,
           (SCM grob),
           R"(
Return the current staff space height in the staff associated with @var{grob},
expressed as a multiple of the default height of a staff space in the
traditional five-line staff.
           )")
{
  auto *const g = LY_ASSERT_SMOB (Grob, grob, 1);
  Real staff_space = Staff_symbol_referencer::staff_space (g);
  return to_scm (staff_space);
}

LY_DEFINE (ly_staff_symbol_staff_radius, "ly:staff-symbol-staff-radius", 1, 0,
           0, (SCM grob),
           R"(
Return the radius of the staff associated with @var{grob}.
           )")
{
  auto *const g = LY_ASSERT_SMOB (Grob, grob, 1);
  Real staff_radius = Staff_symbol_referencer::staff_radius (g);
  return to_scm (staff_radius);
}
