/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2022 Mike Solomon <mike@mikesolomon.org>

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

#include "axis-group-interface.hh"
#include "grob.hh"
#include "grob-array.hh"
#include "moment.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "pure-from-neighbor-interface.hh"
#include "spanner.hh"
#include "system.hh"

using std::vector;

MAKE_SCHEME_CALLBACK (
  Pure_from_neighbor_interface, calc_pure_relevant_grobs,
  "ly:pure-from-neighbor-interface::calc-pure-relevant-grobs", 1);
SCM
Pure_from_neighbor_interface::calc_pure_relevant_grobs (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  extract_grob_set (
    (me->original () && me->original ()->is_live () ? me->original () : me),
    "neighbors", elts);

  vector<Grob *> new_elts;
  new_elts.insert (new_elts.end (), elts.begin (), elts.end ());

  if (Grob_array *a = unsmob<Grob_array> (get_object (me, "neighbors")))
    a->set_array (new_elts);

  return Axis_group_interface::internal_calc_pure_relevant_grobs (me,
                                                                  "neighbors");
}

ADD_INTERFACE (Pure_from_neighbor_interface,
               R"(
A collection of routines to allow for objects' pure heights and heights to be
calculated based on the heights of the objects' neighbors.
               )",

               /* properties */
               R"(
neighbors
pure-relevant-grobs
pure-Y-common
               )");
