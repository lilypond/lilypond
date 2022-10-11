/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020--2022 David Stephen Grant <david@davidgrant.no>

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

MAKE_SCHEME_CALLBACK (Vowel_transition, set_spacing_rods,
                      "ly:vowel-transition::set-spacing-rods", 1);
SCM
Vowel_transition::set_spacing_rods (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Spanner, smob, 1);

  SCM minimum_length = get_property (me, "minimum-length");
  SCM broken_length = get_property (me, "minimum-length-after-break");
  if (scm_is_number (minimum_length) || scm_is_number (broken_length))
    {
      System *root = get_root_system (me);
      const auto bounds = me->get_bounds ();
      if (!bounds[LEFT] || !bounds[RIGHT])
        return SCM_UNSPECIFIED;
      std::vector<Item *> cols (root->broken_col_range (
        bounds[LEFT]->get_column (), bounds[RIGHT]->get_column ()));
      Drul_array<Real> padding = {0.0, 0.0};
      Drul_array<Real> padding_broken = {0.0, 0.0};
      for (const auto d : {LEFT, RIGHT})
        {
          SCM bounds = get_property (me, "bound-details");
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
          Rod rod_before_break;
          rod_before_break.item_drul_[LEFT] = me->get_bound (LEFT);
          rod_before_break.item_drul_[RIGHT]
            = cols[0]->find_prebroken_piece (LEFT);
          rod_before_break.distance_ = from_scm<double> (minimum_length, 0);
          rod_before_break.distance_ += padding[LEFT];
          rod_before_break.distance_ += padding_broken[RIGHT];
          rod_before_break.distance_ += rod_before_break.bounds_protrusion ();
          rod_before_break.add_to_cols ();

          /* After line break */
          Rod rod_after_break;
          rod_after_break.item_drul_[LEFT]
            = cols.back ()->find_prebroken_piece (RIGHT);
          rod_after_break.item_drul_[RIGHT] = me->get_bound (RIGHT);
          Interval_t<Moment> segment_time
            = spanned_time_interval (rod_after_break.item_drul_[LEFT],
                                     rod_after_break.item_drul_[RIGHT]);
          segment_time[LEFT].grace_part_ = 0;
          /*
            Calculate and add space only if the vowel transition is to be drawn.
            I.e., either it does not end on the first note after breaking,
            or property after-line-breaking is set to #t.
          */
          if ((segment_time.length () != Moment (0, 0))
              || from_scm<bool> (get_property_data (me, "after-line-breaking")))
            {
              rod_after_break.distance_
                = (scm_is_number (broken_length)
                     ? from_scm<double> (broken_length, 0)
                     : from_scm<double> (minimum_length, 0));
              rod_after_break.distance_ += padding_broken[LEFT];
              rod_after_break.distance_ += padding[RIGHT];
              rod_after_break.distance_ += rod_after_break.bounds_protrusion ();
              rod_after_break.add_to_cols ();
            }
        }

      Rod rod;
      rod.distance_ = from_scm<double> (minimum_length, 0);
      rod.item_drul_[LEFT] = me->get_bound (LEFT);
      rod.item_drul_[RIGHT] = me->get_bound (RIGHT);
      for (const auto d : {LEFT, RIGHT})
        rod.distance_ += padding[d];
      rod.distance_ += rod.bounds_protrusion ();
      rod.add_to_cols ();

      if (Item *left_pbp = me->get_bound (RIGHT)->find_prebroken_piece (LEFT))
        {
          rod.item_drul_[RIGHT] = left_pbp;
          rod.add_to_cols ();
        }
    }

  return SCM_UNSPECIFIED;
}
