/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "rhythmic-head.hh"

#include "item.hh"
#include "rest.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "warn.hh"

Item *
Rhythmic_head::get_dots (Grob *me)
{
  SCM s = me->get_object ("dot");
  return unsmob<Item> (s);
}

Item *
Rhythmic_head::get_stem (Grob *me)
{
  SCM s = me->get_object ("stem");
  return unsmob<Item> (s);
}

int
Rhythmic_head::dot_count (Grob *me)
{
  return get_dots (me)
             ? robust_scm2int (get_dots (me)->get_property ("dot-count"), 0)
             : 0;
}

void
Rhythmic_head::set_dots (Grob *me, Item *dot)
{
  me->set_object ("dot", dot->self_scm ());
}

int
Rhythmic_head::duration_log (Grob *me)
{
  SCM s = me->get_property ("duration-log");
  return scm_is_number (s) ? scm_to_int (s) : 0;
}

ADD_INTERFACE (Rhythmic_head, "Note head or rest.",

               /* properties */
               "dot "
               "duration-log "
               "glissando-skip "
               "stem ");
