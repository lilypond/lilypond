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

#include "directional-element-interface.hh"
#include "warn.hh"
#include "international.hh"
#include "grob.hh"

Direction
internal_get_grob_direction (Grob *me, bool strict)
{
  SCM d = get_property (me, "direction");
  auto dir = from_scm<Direction> (d, CENTER);
  if (strict && (dir == CENTER))
    {
      me->warning (
        _f ("direction of grob %s must be UP or DOWN; using UP", me->name ()));
      set_grob_direction (me, UP);
      return UP;
    }

  return from_scm<Direction> (d);
}

Direction
get_grob_direction (Grob *me)
{
  return internal_get_grob_direction (me, false);
}

// Use this function when your call site cannot sensibly continue
// with CENTER as a direction (e.g., when using the result as an
// index to a Drul_array), to avoid crashes.  Stay with get_grob_direction
// for grobs for which CENTER is a meaningful direction and the
// absence of an explicitly set direction should be interpreted
// like that.

// TODO: should use the strict version in many more places

// TODO: how to cater for cases where we should use, e.g.,
// get_property_data?

// TODO: add Scheme interface?

Direction
get_strict_grob_direction (Grob *me)
{
  return internal_get_grob_direction (me, true);
}

void
set_grob_direction (Grob *me, Direction d)
{
  SCM sd = to_scm (d);
  set_property (me, "direction", sd);
}
