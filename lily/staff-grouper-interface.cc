/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2009--2022 Joe Neeman <joeneeman@gmail.com>

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

#include <algorithm>

// Find the furthest staff in the given direction whose x-extent overlaps with
// the given interval.
Grob *
Staff_grouper_interface::get_extremal_staff (Grob *me, Grob *refpoint,
                                             Direction dir, Interval const &iv)
{
  // N.B. This is intended to work for a VerticalAlignment grob even though
  // VerticalAlignment does not have the staff-grouper interface.  StaffGrouper
  // and VerticalAlignment grobs are both created by the
  // Vertical_align_engraver and contain elements meeting a common set of
  // criteria, yet they are not described as having a common interface.  Should
  // we treat staff grouping as a subset of vertical alignment?  Should we
  // factor out the shared subset of features into a new interface?

  extract_grob_set (me, "elements", elts);
  vsize start = (dir == UP) ? 0 : elts.size () - 1;
  vsize end = (dir == UP) ? elts.size () : VPOS;
  for (vsize i = start; i != end; i += dir)
    {
      if (has_interface<Hara_kiri_group_spanner> (elts[i]))
        Hara_kiri_group_spanner::consider_suicide (elts[i]);

      Interval intersection = elts[i]->extent (refpoint, X_AXIS);
      intersection.intersect (iv);
      if (elts[i]->is_live () && !intersection.is_empty ())
        return elts[i];
    }
  return 0;
}

/* Checks whether the child grob is in the "interior" of this staff-grouper.
   This is the case if the next spaceable, living child after the given one
   belongs to the group.
*/
bool
Staff_grouper_interface::maybe_pure_within_group (Grob *me, Grob *child,
                                                  bool pure, int start, int end)
{
  extract_grob_set (me, "elements", elts);

  auto i = std::find (elts.begin (), elts.end (), child);
  if (i == elts.end ())
    return false;

  for (++i; i != elts.end (); ++i)
    if (Page_layout_problem::is_spaceable (*i)
        && ((pure && !Hara_kiri_group_spanner::request_suicide (*i, start, end))
            || (!pure && (*i)->is_live ())))
      return me == unsmob<Grob> (get_object (*i, "staff-grouper"));

  // If there was no spaceable, living child after me, I don't
  // count as within the group.
  return false;
}

ADD_INTERFACE (Staff_grouper_interface,
               R"(
A grob that collects staves together.
               )",

               /* properties */
               R"(
staff-staff-spacing
staffgroup-staff-spacing
               )");
