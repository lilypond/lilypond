/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2012 Mike Solomon <mike@apollinemike.com>

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

MAKE_SCHEME_CALLBACK (Pure_from_neighbor_interface, calc_pure_relevant_grobs, 1);
SCM
Pure_from_neighbor_interface::calc_pure_relevant_grobs (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set ((me->original () && me->original ()->is_live ()
                     ? me->original ()
                     : me),
                    "neighbors",
                    elts);

  vector<Grob *> new_elts;
  new_elts.insert (new_elts.end (), elts.begin (), elts.end ());

  SCM neighbors_scm = me->get_object ("neighbors");
  if (Grob_array::unsmob (neighbors_scm))
    {
      vector<Grob *> &arr
        = unsmob_grob_array (neighbors_scm)->array_reference ();
      arr = new_elts;
    }

  return Axis_group_interface::internal_calc_pure_relevant_grobs (me, "neighbors");
}

ADD_INTERFACE (Pure_from_neighbor_interface,
               "A collection of routines to allow for objects' pure"
               "heights and heights to be calculated based on the"
               "heights of the objects' neighbors.",

               /* properties */
               "neighbors "
               "pure-relevant-grobs "
               "pure-Y-common "
              );
