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

MAKE_SCHEME_CALLBACK (Fingering_column, calc_positioning_done, 1);
SCM
Fingering_column::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (!me->is_live ())
    return SCM_BOOL_T;

  me->set_property ("positioning-done", SCM_BOOL_T);

  extract_grob_set (me, "fingerings", const_fingerings);

  if (const_fingerings.size () < 2)
    {
      me->programming_error ("This FingeringColumn should have never been created.");
      return SCM_BOOL_T;
    }

  vector<Grob *> fingerings;
  for (vsize i = 0; i < const_fingerings.size (); i++)
    fingerings.push_back (const_fingerings[i]);


  Grob *common[2] = {common_refpoint_of_array (fingerings, me, X_AXIS),
                     common_refpoint_of_array (fingerings, me, Y_AXIS)};

  Real padding = robust_scm2double (me->get_property ("padding"), 0.2);

  // order the fingerings from bottom to top
  vector_sort (fingerings, pure_position_less);

 vector<Real> shift(fingerings.size());

  // Try stacking the fingerings top-to-bottom, and then bottom-to-top.
  // Use the average of the resulting stacked locations as the final positions
  for (UP_and_DOWN (d))
     {
      Real stack_end = -d * infinity_f;
      Interval prev_x_ext;
      for (vsize i = (d == UP)? 0 : fingerings.size() - 1;
           i < fingerings.size ();
           i += d)
        {
          Interval x_ext = fingerings[i]->extent(common[X_AXIS], X_AXIS);
          Interval y_ext = fingerings[i]->extent(fingerings[i], Y_AXIS);
          Real parent_y = fingerings[i]->get_parent(Y_AXIS)
                         ->relative_coordinate(common[Y_AXIS], Y_AXIS);

          // Checking only between sequential neighbors, seems good enough
          if (!intersection(x_ext, prev_x_ext).is_empty())
            stack_end += d * (y_ext.length() + padding);
          // minmax() returns whichever is further along in direction d
          stack_end = minmax(d, stack_end, parent_y);

          shift[i] += 0.5 * (stack_end - y_ext[d] - parent_y);

          prev_x_ext = x_ext;
        }
     }

  for (vsize i = 0; i < fingerings.size (); i++)
    fingerings[i]->translate_axis(shift[i], Y_AXIS);

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
