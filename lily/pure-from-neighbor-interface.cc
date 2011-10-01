/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011 Mike Solomon <mike@apollinemike.com>

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
#include "grob-array.hh"
#include "pointer-group-interface.hh"
#include "pure-from-neighbor-interface.hh"
#include "spanner.hh"
#include "system.hh"

MAKE_SCHEME_CALLBACK (Pure_from_neighbor_interface, filter_elements, 1);
SCM
Pure_from_neighbor_interface::filter_elements (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "elements", elts);
  vector<Grob *> new_elts;
  Interval_t<int> srl = me->get_system ()->spanned_rank_interval ();
  for (vsize i = 0; i < elts.size (); i++)
    if (srl.contains (elts[i]->spanned_rank_interval ()[LEFT]))
      new_elts.push_back (elts[i]);

  SCM elements_scm = me->get_object ("elements");
  if (Grob_array::unsmob (elements_scm))
    {
      vector<Grob *> &arr
        = unsmob_grob_array (elements_scm)->array_reference ();
      arr = new_elts;
    }

  return SCM_BOOL_T;
}

ADD_INTERFACE (Pure_from_neighbor_interface,
               "A collection of routines to allow for objects' pure"
               "heights and heights to be calculated based on the"
               "heights of the objects' neighbors.",

               /* properties */
               "elements-filtered "
              );
