/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012 Mike Solomon <mike@mikesolomon.org>

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
#include "fingering-column.hh"
#include "pointer-group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"
#include "paper-column.hh"

#include <map>

MAKE_SCHEME_CALLBACK (Fingering_column, calc_positioning_done, 1);
SCM
Fingering_column::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Real padding = robust_scm2double (me->get_property ("padding"), 0.0);
  if (!me->is_live ())
    return SCM_BOOL_T;

  map<Grob *, bool> shifted;

  Real ss = Staff_symbol_referencer::staff_space (me);

  me->set_property ("positioning-done", SCM_BOOL_T);

  extract_grob_set (me, "fingerings", const_fingerings);

  if (const_fingerings.size () < 2)
    {
      me->programming_error ("This FingeringColumn should have never been created.");
      return SCM_BOOL_T;
    }

  // order the fingerings from bottom to top
  vector<Grob *> fingerings;
  for (vsize i = 0; i < const_fingerings.size (); i++)
    fingerings.push_back (const_fingerings[i]);

  vector_sort (fingerings, pure_position_less);

  Grob *common[2] = {common_refpoint_of_array (fingerings, me, X_AXIS),
                     common_refpoint_of_array (fingerings, me, Y_AXIS)};

  for (vsize i = 0; i < fingerings.size (); i++)
    fingerings[i]->translate_axis (-fingerings[i]->extent (common[Y_AXIS], Y_AXIS).length () / 2, Y_AXIS);

  for (vsize i = min (fingerings.size () - 1, fingerings.size () / 2 + 1); i >= 1; i--)
    for (vsize j = i; j--;)
      {
        Interval ex_i = fingerings[i]->extent (common[X_AXIS], X_AXIS);
        Interval ex_j = fingerings[j]->extent (common[X_AXIS], X_AXIS);
        Interval ey_i = fingerings[i]->extent (common[Y_AXIS], Y_AXIS);
        Interval ey_j = fingerings[j]->extent (common[Y_AXIS], Y_AXIS);
        Real tval = min (0.0, (ey_i[DOWN] - ey_j[UP] - padding) / 2);
        if (tval != 0.0 && !intersection (ex_i, ex_j).is_empty ())
          {
            if (shifted[fingerings[i]] || shifted[fingerings[j]])
              fingerings[j]->translate_axis (tval * 2, Y_AXIS);
            else
              {
                fingerings[i]->translate_axis (-tval, Y_AXIS);
                fingerings[j]->translate_axis (tval, Y_AXIS);
              }
            shifted[fingerings[i]] = true;
            shifted[fingerings[j]] = true;
          }
      }

  for (vsize i = fingerings.size () / 2 - 1; i < fingerings.size () - 1; i++)
    for (vsize j = i + 1; j < fingerings.size (); j++)
      {
        Interval ex_i = fingerings[i]->extent (common[X_AXIS], X_AXIS);
        Interval ex_j = fingerings[j]->extent (common[X_AXIS], X_AXIS);
        Interval ey_i = fingerings[i]->extent (common[Y_AXIS], Y_AXIS);
        Interval ey_j = fingerings[j]->extent (common[Y_AXIS], Y_AXIS);
        Real tval = max (0.0, (ey_i[UP] - ey_j[DOWN] + padding) / 2);
        if (tval != 0.0 && !intersection (ex_i, ex_j).is_empty ())
          {
            if (shifted[fingerings[i]] || shifted[fingerings[j]])
              fingerings[j]->translate_axis (tval * 2, Y_AXIS);
            else
              {
                fingerings[i]->translate_axis (-tval, Y_AXIS);
                fingerings[j]->translate_axis (tval, Y_AXIS);
              }
            shifted[fingerings[i]] = true;
            shifted[fingerings[j]] = true;
          }
      }


  return SCM_BOOL_T;
}

void
Fingering_column::add_fingering (Grob *fc, Grob *f)
{
  Pointer_group_interface::add_grob (fc, ly_symbol2scm ("fingerings"), f);
  f->set_parent (fc, X_AXIS);
  f->set_property ("Y-offset", Grob::x_parent_positioning_proc);
}

ADD_INTERFACE (Fingering_column,
               "Makes sure that fingerings placed laterally"
               " do not collide.",

               /* properties */
               "padding "
               "positioning-done "
              );
