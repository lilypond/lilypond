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

#include "directional-element-interface.hh"
#include "warn.hh"
#include "grob.hh"

Direction
get_grob_direction (Grob *me)
{
  SCM d = me->get_property ("direction");
  if (d == ly_symbol2scm ("calculation-in-progress"))
    {
      programming_error ("Grob direction requested while calculation in progress. ");
      return UP;
    }
  if (!is_direction (d))
    return CENTER;

  return to_dir (d);
}

void
set_grob_direction (Grob *me, Direction d)
{
  SCM sd = scm_from_int (d);
  me->set_property ("direction", sd);
}
