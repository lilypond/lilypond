/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "system.hh"
#include "paper-column.hh"
#include "column-x-positions.hh"
#include "pointer-group-interface.hh"
#include "staff-spacing.hh"
#include "note-spacing.hh"
#include "spacing-spanner.hh"
#include "warn.hh"
#include "moment.hh"
#include "spacing-options.hh"

using std::vector;

/* Find the loose columns in POSNS, and drape them around the columns
   specified in BETWEEN-COLS.  */
void
set_loose_columns (System *which, Column_x_positions const *posns)
{
  vsize loose_col_count = posns->loose_cols_.size ();
  if (!loose_col_count)
    return;

  for (vsize i = 0; i < loose_col_count; i++)
    posns->loose_cols_[i]->set_system (which);

  for (vsize i = 0; i < loose_col_count; i++)
    {
      int divide_over = 1;
      Paper_column *loose = static_cast<Paper_column *> (posns->loose_cols_[i]);

      Paper_column *left = 0;
      Paper_column *right = 0;

      vector<Paper_column *> clique;
      while (1)
        {
          SCM between = get_object (loose, "between-cols");
          if (!scm_is_pair (between))
            break;

          /* If the line was broken at one of the loose columns, split
             the clique at that column. */
          if (!loose->get_system ())
            break;

          Paper_column *le = unsmob<Paper_column> (scm_car (between));
          Paper_column *re = unsmob<Paper_column> (scm_cdr (between));

          if (!(le && re))
            break;

          if (!left && le)
            {
              left = le->get_column ();
              if (!left->get_system ())
                left = left->find_prebroken_piece (RIGHT);

              clique.push_back (left);
            }

          clique.push_back (loose);

          divide_over++;
          loose = right = re->get_column ();
        }

      if (!right)
        {
          programming_error ("Can't attach loose column sensibly."
                             "  Attaching to end of system.");
          right = which->get_bound (RIGHT);
        }

      if (right->get_system ())
        ; /* do nothing */
      else if (right->find_prebroken_piece (LEFT)
               && right->find_prebroken_piece (LEFT)->get_system () == which)
        right = right->find_prebroken_piece (LEFT);
      else if (which->get_bound (RIGHT)->get_rank () < right->get_rank ())
        right = which->get_bound (RIGHT);
      else
        {
          clique.back ()->programming_error (
            "Loose column does not have right side to attach to.");
          System *base_system = which->original ();
          int j = clique.back ()->get_rank () + 1;
          int end_rank = which->get_bound (RIGHT)->get_rank ();
          extract_grob_set (base_system, "columns", base_cols);
          for (; j < end_rank; j++)
            {
              if (base_cols[j]->get_system () == which)
                right = dynamic_cast<Paper_column *> (base_cols[j]);
            }
        }

      Grob *common = right->common_refpoint (left, X_AXIS);

      clique.push_back (right);

      /*
        We use two vectors to keep track of loose column spacing:
          clique_spacing keeps track of ideal spaces.
          clique_tight_spacing keeps track of minimum spaces.
        Below, a scale factor is applied to the shifting of loose columns that
        aims to preserve clique_spacing but gets closer to clique_tight_spacing as the
        space becomes smaller.  This is used because the rods placed for loose columns
        are tight (meaning they use minimum distances - see set_distances_for_loose_columns).
        However, other rods may widen this distance, in which case we don't want a crammed score.
        Thus, we aim for non-crammed, and fall back on crammed as needed.
      */
      vector<Real> clique_spacing;
      vector<Real> clique_tight_spacing;
      clique_spacing.push_back (0.0);
      clique_tight_spacing.push_back (0.0);
      for (vsize j = 1; j + 1 < clique.size (); j++)
        {
          Grob *clique_col = clique[j];

          Paper_column *loose_col = clique[j];
          Paper_column *next_col = clique[j + 1];

          Grob *spacing = unsmob<Grob> (get_object (clique_col, "spacing"));
          if (Grob *grace_spacing
              = unsmob<Grob> (get_object (clique_col, "grace-spacing")))
            {
              spacing = grace_spacing;
            }

          Spacing_options options;
          if (spacing)
            options.init_from_grob (spacing);
          else
            programming_error ("Column without spacing object");

          Spring spring;
          if (Paper_column::is_musical (next_col)
              && Paper_column::is_musical (loose_col))
            {
              if (has_interface<Note_spacing> (spacing))
                spring = Note_spacing::get_spacing (spacing, next_col, spring,
                                                    options.increment_);
              else
                spring = Spacing_spanner::note_spacing (spacing, loose_col,
                                                        next_col, &options);
            }
          else
            {
              spring = Spacing_spanner::standard_breakable_column_spacing (
                spacing, loose_col, next_col, &options);
            }

          Real base_note_space = spring.ideal_distance ();
          Real tight_note_space = spring.min_distance ();

          Real loose_col_horizontal_length
            = loose_col->extent (loose_col, X_AXIS).length ();
          base_note_space
            = std::max (base_note_space, loose_col_horizontal_length);
          tight_note_space
            = std::max (tight_note_space, loose_col_horizontal_length);

          clique_spacing.push_back (base_note_space);
          clique_tight_spacing.push_back (tight_note_space);
        }

      Real permissible_distance
        = clique.back ()->relative_coordinate (common, X_AXIS)
          - robust_relative_extent (clique[0], common, X_AXIS)[RIGHT];
      Real right_point
        = robust_relative_extent (clique.back (), common, X_AXIS)[LEFT];
      Grob *finished_right_column = clique.back ();

      Real sum_tight_spacing = 0;
      Real sum_spacing = 0;
      // currently a magic number - what would be a good grob to hold this property?
      Real left_padding = 0.15;
      for (vsize j = 0; j < clique_spacing.size (); j++)
        {
          sum_tight_spacing += clique_tight_spacing[j];
          sum_spacing += clique_spacing[j];
        }
      Real scale_factor = std::max (
        0.0,
        std::min (1.0, (permissible_distance - left_padding - sum_tight_spacing)
                         / (sum_spacing - sum_tight_spacing)));
      for (vsize j = clique.size () - 2; j > 0; j--)
        {
          Paper_column *clique_col = clique[j];

          right_point
            = finished_right_column->relative_coordinate (common, X_AXIS);

          Real distance_to_next
            = clique_tight_spacing[j]
              + (clique_spacing[j] - clique_tight_spacing[j]) * scale_factor;

          Real my_offset = right_point - distance_to_next;

          clique_col->translate_axis (
            my_offset - clique_col->relative_coordinate (common, X_AXIS),
            X_AXIS);

          finished_right_column = clique_col;
        }
    }
}
