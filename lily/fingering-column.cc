/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2020 Mike Solomon <mike@mikesolomon.org>

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

#include "fingering-column.hh"
#include "directional-element-interface.hh"
#include "grob.hh"
#include "item.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "staff-symbol-referencer.hh"

using std::vector;

#define EPS 1e-5

struct Fingering_and_offset
{
  Grob *fingering_;
  Real offset_;
  Fingering_and_offset (Grob *fingering, Real offset);
};

Fingering_and_offset::Fingering_and_offset (Grob *fingering, Real offset)
    : fingering_ (fingering), offset_ (offset)
{
}

bool
fingering_and_offset_less (Fingering_and_offset fo0, Fingering_and_offset fo1)
{
  return fo0.offset_ < fo1.offset_;
}

MAKE_SCHEME_CALLBACK (Fingering_column, calc_positioning_done, 1);
SCM
Fingering_column::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  if (!me->is_live ())
    return SCM_BOOL_T;

  me->set_property ("positioning-done", SCM_BOOL_T);

  do_y_positioning (me);
  do_x_positioning (me);

  return SCM_BOOL_T;
}

void
Fingering_column::do_y_positioning (Grob *me)
{
  extract_grob_set (me, "fingerings", const_fingerings);

  if (const_fingerings.size () < 2)
    {
      me->programming_error (
          "This FingeringColumn should have never been created.");
      return;
    }

  vector<Grob *> fingerings;
  for (vsize i = 0; i < const_fingerings.size (); i++)
    fingerings.push_back (const_fingerings[i]);

  Grob *common[2] = {common_refpoint_of_array (fingerings, me, X_AXIS),
                     common_refpoint_of_array (fingerings, me, Y_AXIS)};

  Real padding = robust_scm2double (me->get_property ("padding"), 0.2);

  // order the fingerings from bottom to top
  vector_sort (fingerings, pure_position_less);

  vector<Real> shift (fingerings.size ());

  // Try stacking the fingerings top-to-bottom, and then bottom-to-top.
  // Use the average of the resulting stacked locations as the final positions
  for (UP_and_DOWN (d))
    {
      Real stack_end = -d * infinity_f;
      Interval prev_x_ext;
      for (vsize i = (d == UP) ? 0 : fingerings.size () - 1;
           i < fingerings.size (); i += d)
        {
          Interval x_ext = fingerings[i]->extent (common[X_AXIS], X_AXIS);
          Interval y_ext = fingerings[i]->extent (fingerings[i], Y_AXIS);
          Real parent_y
              = fingerings[i]->parent_relative (common[Y_AXIS], Y_AXIS);

          // Checking only between sequential neighbors, seems good enough
          if (!intersection (x_ext, prev_x_ext).is_empty ())
            stack_end += d * (y_ext.length () + padding);
          // minmax() returns whichever is further along in direction d
          stack_end = minmax (d, stack_end, parent_y);

          shift[i] += 0.5 * (stack_end - y_ext[d] - parent_y);

          prev_x_ext = x_ext;
        }
    }

  for (vsize i = 0; i < fingerings.size (); i++)
    fingerings[i]->translate_axis (shift[i], Y_AXIS);
}

void
Fingering_column::do_x_positioning (Grob *me)
{
  extract_grob_set (me, "fingerings", fingerings);
  if (!fingerings.size ())
    return;

  Grob *common_x = common_refpoint_of_array (fingerings, me, X_AXIS);

  Real snap = robust_scm2double (me->get_property ("snap-radius"), 0.3);
  vector<Fingering_and_offset> fos;

  for (vsize i = 0; i < fingerings.size (); i++)
    fos.push_back (Fingering_and_offset (
        fingerings[i], fingerings[i]->relative_coordinate (common_x, X_AXIS)));

  vector_sort (fos, fingering_and_offset_less);
  Direction dir = get_grob_direction (fingerings[0]);
  if (dir == RIGHT)
    reverse (fos);

  Real prev = infinity_f * dir;
  for (vsize i = 0; i < fos.size (); i++)
    {
      if ((fabs (fos[i].offset_ - prev) < snap)
          && (fabs (fos[i].offset_ - prev) > EPS))
        fos[i].offset_ = prev;

      prev = fos[i].offset_;
    }

  for (vsize i = 0; i < fos.size (); i++)
    fos[i].fingering_->translate_axis (
        fos[i].offset_
            - fos[i].fingering_->relative_coordinate (common_x, X_AXIS),
        X_AXIS);
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
               " do not collide and that they are flush if"
               " necessary.",

               /* properties */
               "padding "
               "positioning-done "
               "snap-radius ");
