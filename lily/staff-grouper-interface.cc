/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2009--2012 Joe Neeman <joeneeman@gmail.com>

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

#include "staff-grouper-interface.hh"

#include "hara-kiri-group-spanner.hh"
#include "page-layout-problem.hh"
#include "pointer-group-interface.hh"

/* Checks whether the child grob is in the "interior" of this staff-grouper.
   This is the case if the next spaceable, living child after the given one
   belongs to the group.
*/
bool
Staff_grouper_interface::maybe_pure_within_group (Grob *me, Grob *child, bool pure, int start, int end)
{
  extract_grob_set (me, "elements", elts);

  vector<Grob *>::const_iterator i = find (elts, child);

  if (i == elts.end ())
    return false;

  for (++i; i != elts.end (); ++i)
    if (Page_layout_problem::is_spaceable (*i)
        && ((pure && !Hara_kiri_group_spanner::request_suicide (*i, start, end))
            || (!pure && (*i)->is_live ())))
      return me == unsmob_grob ((*i)->get_object ("staff-grouper"));

  // If there was no spaceable, living child after me, I don't
  // count as within the group.
  return false;
}

ADD_INTERFACE (Staff_grouper_interface,
               "A grob that collects staves together.",

               /* properties */
               "staff-staff-spacing "
               "staffgroup-staff-spacing "
              );

