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
#include "box-quarantine.hh"
#include "pointer-group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"
#include "paper-column.hh"

#include <map>

// The sorting algorithm may not preserve the order of the
// original fingerings. We use Fingering_position_info to retain
// this order.

struct Fingering_position_info {
  Box box_;
  vsize idx_;
  Fingering_position_info (Box, vsize);
};

Fingering_position_info::Fingering_position_info (Box box, vsize idx)
  : box_ (box), idx_ (idx)
{
}

bool
fingering_position_less (Fingering_position_info fpi0, Fingering_position_info fpi1)
{
  return Interval::left_less (fpi0.box_[Y_AXIS], fpi1.box_[Y_AXIS]);
}

MAKE_SCHEME_CALLBACK (Fingering_column, calc_positioning_done, 1);
SCM
Fingering_column::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (!me->is_live ())
    return SCM_BOOL_T;

  map<Grob *, bool> shifted;

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
  Box_quarantine bq (padding, Y_AXIS);
  vector<Fingering_position_info> origs;
  for (vsize i = 0; i < fingerings.size (); i++)
    {
      Interval x_ext = fingerings[i]->extent (common[X_AXIS], X_AXIS);
      // center on Y parent
      fingerings[i]->translate_axis (-fingerings[i]->extent (fingerings[i], Y_AXIS).length () / 2.0, Y_AXIS);
      Interval y_ext = fingerings[i]->extent (fingerings[i], Y_AXIS)
                       + fingerings[i]->get_parent (Y_AXIS)
                         ->relative_coordinate (common[Y_AXIS], Y_AXIS);
      origs.push_back(Fingering_position_info (Box (x_ext, y_ext), i));
    }

  // order the fingerings from bottom to top
  vector_sort (origs, fingering_position_less);

  for (vsize i = 0; i < origs.size (); i++)
    bq.add_box_to_quarantine (Box (origs[i].box_));

  bq.solve ();
  vector<Box> news (bq.quarantined_boxes ());

  for (vsize i = 0; i < origs.size (); i++)
    fingerings[origs[i].idx_]->translate_axis (news[i][Y_AXIS][DOWN] - origs[i].box_[Y_AXIS][DOWN], Y_AXIS);

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
