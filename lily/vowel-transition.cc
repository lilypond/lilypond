/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020--2020 David Stephen Grant <david@davidgrant.no>

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

#include "vowel-transition.hh"

#include "axis-group-interface.hh"
#include "lookup.hh"
#include "moment.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "rod.hh"
#include "spanner.hh"
#include "system.hh"

MAKE_SCHEME_CALLBACK (Vowel_transition, set_spacing_rods, 1);
SCM
Vowel_transition::set_spacing_rods (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  SCM minimum_length = get_property (me, "minimum-length");
  SCM broken_length = get_property (me, "minimum-length-after-break");
  if (scm_is_number (minimum_length) || scm_is_number (broken_length))
    {
      Spanner *sp = dynamic_cast<Spanner *> (me);
      System *root = get_root_system (me);
      Drul_array<Item *> bounds (sp->get_bound (LEFT), sp->get_bound (RIGHT));
      if (!bounds[LEFT] || !bounds[RIGHT])
        return SCM_UNSPECIFIED;
      std::vector<Item *> cols (root->broken_col_range (
          bounds[LEFT]->get_column (), bounds[RIGHT]->get_column ()));
      Drul_array<Real> padding = {0.0, 0.0};
      Drul_array<Real> padding_broken = {0.0, 0.0};
      for (LEFT_and_RIGHT (d))
        {
          SCM bounds = get_property (sp, "bound-details");
          SCM details = ly_assoc_get (
              (d == LEFT ? ly_symbol2scm ("left") : ly_symbol2scm ("right")),
              bounds, SCM_BOOL_F);
          SCM details_broken
              = ly_assoc_get ((d == LEFT ? ly_symbol2scm ("left-broken")
                                         : ly_symbol2scm ("right-broken")),
                              bounds, SCM_BOOL_F);
          if (!scm_is_false (details))
            padding[d] = from_scm<double> (
                ly_assoc_get (ly_symbol2scm ("padding"), details, SCM_BOOL_F),
                0.0);
          if (!scm_is_false (details_broken))
            padding_broken[d]
                = from_scm<double> (ly_assoc_get (ly_symbol2scm ("padding"),
                                                   details_broken, SCM_BOOL_F),
                                     0.0);
        }

      if (cols.size ())
        {
          /* Before line break */
          Rod r1;
          r1.item_drul_[LEFT] = sp->get_bound (LEFT);
          r1.item_drul_[RIGHT] = cols[0]->find_prebroken_piece (LEFT);
          r1.distance_ = from_scm<double> (minimum_length, 0);
          r1.distance_ += padding[LEFT];
          r1.distance_ += padding_broken[RIGHT];
          r1.distance_ += bounds_protrusion (&r1);
          r1.add_to_cols ();

          /* After line break */
          Rod r2;
          r2.item_drul_[LEFT] = cols.back ()->find_prebroken_piece (RIGHT);
          r2.item_drul_[RIGHT] = sp->get_bound (RIGHT);
          Interval_t<Moment> segment_time = spanned_time_interval (
              r2.item_drul_[LEFT], r2.item_drul_[RIGHT]);
          segment_time[LEFT].grace_part_ = 0;
          /*
            Calculate and add space only if the vowel transition is to be drawn.
            I.e., either it does not end on the first note after breaking,
            or property after-line-breaking is set to #t.
          */
          if ((segment_time.length () != Moment (0, 0))
              || from_scm<bool> (get_property (me, "after-line-breaking")))
            {
              r2.distance_ = (scm_is_number (broken_length)
                                  ? from_scm<double> (broken_length, 0)
                                  : from_scm<double> (minimum_length, 0));
              r2.distance_ += padding_broken[LEFT];
              r2.distance_ += padding[RIGHT];
              r2.distance_ += bounds_protrusion (&r2);
              r2.add_to_cols ();
            }
        }

      Rod r;
      r.distance_ = from_scm<double> (minimum_length, 0);
      r.item_drul_[LEFT] = sp->get_bound (LEFT);
      r.item_drul_[RIGHT] = sp->get_bound (RIGHT);
      for (LEFT_and_RIGHT (d))
        r.distance_ += padding[d];
      r.distance_ += bounds_protrusion (&r);
      r.add_to_cols ();

      if (Item *left_pbp = sp->get_bound (RIGHT)->find_prebroken_piece (LEFT))
        {
          r.item_drul_[RIGHT] = left_pbp;
          r.add_to_cols ();
        }
    }

  return SCM_UNSPECIFIED;
}

Real
Vowel_transition::bounds_protrusion (const Rod *r)
{
  /* Calculate protrusion of bounds into rod */
  Real w = 0;
  for (LEFT_and_RIGHT (d))
    {
      if (r->item_drul_[d])
        w += -d * r->item_drul_[d]->extent (r->item_drul_[d], X_AXIS)[-d];
    }
  return w;
}