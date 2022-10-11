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

#include "staff-spacing.hh"

#include "spacing-options.hh"
#include "system.hh"
#include "paper-column.hh"
#include "column-x-positions.hh"
#include "pointer-group-interface.hh"
#include "spacing-interface.hh"
#include "spacing-spanner.hh"
#include "note-spacing.hh"
#include "moment.hh"
#include "grob-array.hh"
#include "break-align-interface.hh"
#include "warn.hh"

using std::vector;

/*
  Return whether COL is fixed to its neighbors by some kind of spacing
  constraint.


  If in doubt, then we're not loose; the spacing engine should space
  for it, risking suboptimal spacing.

  (Otherwise, we might risk core dumps, and other weird stuff.)
*/
static bool
is_loose_column (Paper_column *l, Paper_column *col, Paper_column *r,
                 Spacing_options const *options)
{
  if (!from_scm<bool> (get_property (col, "allow-loose-spacing")))
    return false;

  if ((options->float_nonmusical_columns_ || options->float_grace_columns_)
      && Paper_column::when_mom (col).grace_part_)
    {
      return true;
    }

  if (Paper_column::is_musical (col))
    return false;

  /*
    If this column doesn't have a proper neighbor, we should really
    make it loose, but spacing it correctly is more than we can
    currently can handle.

    (this happens in the following situation:

    |
    |    clef G
    *

    |               |      ||
    |               |      ||
    O               O       ||


    the column containing the clef is really loose, and should be
    attached right to the first column, but that is a lot of work for
    such a borderline case.)
  */

  auto *const r_neighbor
    = unsmob<Paper_column> (get_object (col, "right-neighbor"));
  if (!r_neighbor)
    return false;

  auto *const l_neighbor
    = unsmob<Paper_column> (get_object (col, "left-neighbor"));
  if (!l_neighbor)
    return false;

  /* If a non-empty column (ie. not \bar "") is placed nicely in series with
     its neighbor (ie. no funny polyphonic stuff), don't make it loose.
  */
  if ((l == l_neighbor) && (r == r_neighbor)
      && (col->extent (col, X_AXIS).length () > 0))
    {
      return false;
    }

  /*
    Only declare loose if the bounds make a little sense.  This means
    some cases (two isolated, consecutive clef changes) won't be
    nicely folded, but hey, then don't do that.
  */
  auto is_sensible = [] (Paper_column *neighbor) {
    return Paper_column::is_musical (neighbor)
           || Paper_column::is_breakable (neighbor);
  };

  if (!is_sensible (l_neighbor) || !is_sensible (r_neighbor))
    return false;

  /*
    in any case, we don't want to move bar lines.
  */
  if (auto *break_alignment
      = unsmob<Item> (get_object (col, "break-alignment")))
    {
      auto *const staff_bar_group
        = Break_alignment_interface::find_nonempty_break_align_group (
          break_alignment, ly_symbol2scm ("staff-bar"));
      if (staff_bar_group)
        {
          if (staff_bar_group->extent (staff_bar_group, X_AXIS).length () > 0)
            {
              return false;
            }
        }
    }

  return true;
}

void
Spacing_spanner::set_distances_for_loose_col (Grob *me, Grob *c,
                                              Drul_array<Item *> next_door,
                                              Spacing_options const *options)
{
  Drul_array<Real> dists;

  for (const auto d : {LEFT, RIGHT})
    {
      Paper_column *lc
        = dynamic_cast<Paper_column *> ((d == LEFT) ? next_door[LEFT] : c);
      Paper_column *rc
        = dynamic_cast<Paper_column *> (d == LEFT ? c : next_door[RIGHT]);

      extract_grob_set (lc, "spacing-wishes", wishes);
      for (vsize k = wishes.size (); k--;)
        {
          Grob *sp = wishes[k];
          if (Spacing_interface::left_column (sp) != lc
              || Spacing_interface::right_column (sp) != rc)
            continue;

          if (has_interface<Note_spacing> (sp))
            {
              /*
                The note spacing should be taken from the musical
                columns.
              */
              Spring base = note_spacing (me, lc, rc, options);
              Spring spring
                = Note_spacing::get_spacing (sp, rc, base, options->increment_);

              dists[d] = std::max (dists[d], spring.min_distance ());
            }
          else if (has_interface<Staff_spacing> (sp))
            {
              Spring spring = Staff_spacing::get_spacing (sp, rc, 0.0);

              dists[d] = std::max (dists[d], spring.min_distance ());
            }
          else
            programming_error ("Subversive spacing wish");
        }
    }

  Rod r;
  r.distance_ = dists[LEFT] + dists[RIGHT];
  r.item_drul_ = next_door;

  r.add_to_cols ();
}

/*
  Remove columns that are not tightly fitting from COLS. In the
  removed columns, set 'between-cols to the columns where it is in
  between.
*/
void
Spacing_spanner::prune_loose_columns (Grob *me, vector<Paper_column *> *cols,
                                      Spacing_options *options)
{
  // rp is a post-increment read pointer running over the *cols
  // vector, wp is a post-increment write pointer.  They start in sync
  // but become different once a loose column gets pruned.
  vector<Paper_column *>::const_iterator rp;
  vector<Paper_column *>::iterator wp;
  // We keep track of the last column in a separate variable instead
  // of reading it from rp since it could already have been
  // overwritten via wp.  Very strictly speaking, this can only happen
  // when rp and wp are still in lockstep and thus the overwritten
  // value would be unchanged, but let's not get too icky but stick
  // with a pattern that works for more use cases.
  Paper_column *lastcol = 0;
  for (rp = wp = cols->begin (); rp != cols->end ();)
    {
      Paper_column *c = *rp++;

      bool loose = (lastcol && rp != cols->end ()
                    && is_loose_column (lastcol, c, *rp, options));

      /* Breakable columns never get pruned; even if they are loose,
        their broken pieces are not.  However, we mark them so that
        the spacing can take their mid-line looseness into account. */
      if (loose && Paper_column::is_breakable (c))
        {
          loose = false;
          set_property (c, "maybe-loose", SCM_BOOL_T);
        }
      /*
        Unbreakable columns which only contain page-labels also
        never get pruned, otherwise the labels are lost before they can
        be collected by the System: so we mark these columns too.
      */
      if (!loose && !Paper_column::is_breakable (c)
          && scm_is_pair (get_property (c, "labels")))
        {
          extract_grob_set (c, "elements", elts);
          if (elts.empty ())
            set_property (c, "maybe-loose", SCM_BOOL_T);
        }

      if (loose)
        {
          Grob *right_neighbor
            = unsmob<Grob> (get_object (c, "right-neighbor"));
          Grob *left_neighbor = unsmob<Grob> (get_object (c, "left-neighbor"));

          /*
            Either object can be non existent, if the score ends
            prematurely.
          */
          if (!right_neighbor || !left_neighbor)
            {
              c->programming_error (
                "Cannot determine neighbors for floating column.");
              set_object (c, "between-cols",
                          scm_cons (lastcol->self_scm (), (*rp)->self_scm ()));
            }
          else
            {
              set_object (c, "between-cols",
                          scm_cons (left_neighbor->self_scm (),
                                    right_neighbor->self_scm ()));

              /*
                Set distance constraints for loose columns
              */
              Drul_array<Item *> next_door (
                dynamic_cast<Item *> (left_neighbor),
                dynamic_cast<Item *> (right_neighbor));

              set_distances_for_loose_col (me, c, next_door, options);
            }
        }

      else
        *wp++ = c;

      lastcol = c;
    }

  cols->erase (wp, cols->end ());
}

/*
  Set neighboring columns determined by the spacing-wishes grob property.
*/
void
Spacing_spanner::set_explicit_neighbor_columns (
  vector<Paper_column *> const &cols)
{
  for (vsize i = 0; i < cols.size (); i++)
    {
      extract_grob_set (cols[i], "spacing-wishes", wishes);
      for (vsize j = wishes.size (); j--;)
        {
          Item *wish = dynamic_cast<Item *> (wishes[j]);
          Paper_column *left_col = wish->get_column ();
          int left_rank = left_col->get_rank ();
          int min_right_rank = INT_MAX;

          extract_grob_set (wish, "right-items", right_items);
          for (vsize k = right_items.size (); k--;)
            {
              Paper_column *right_col
                = dynamic_cast<Item *> (right_items[k])->get_column ();
              int right_rank = right_col->get_rank ();

              if (right_rank < min_right_rank)
                {
                  set_object (left_col, "right-neighbor",
                              right_col->self_scm ());
                  min_right_rank = right_rank;
                }

              Paper_column *old_left_neighbor = unsmob<Paper_column> (
                get_object (right_col, "left-neighbor"));
              if (!old_left_neighbor
                  || left_rank > old_left_neighbor->get_rank ())
                set_object (right_col, "left-neighbor", left_col->self_scm ());
            }
        }
    }
}

/*
  Set neighboring columns that have no left/right-neighbor set
  yet. Only do breakable non-musical columns, and musical columns.
  Why only these? --jneem
*/
void
Spacing_spanner::set_implicit_neighbor_columns (
  vector<Paper_column *> const &cols)
{
  for (vsize i = 0; i < cols.size (); i++)
    {
      Paper_column *it = cols[i];
      if (!Paper_column::is_breakable (it) && !Paper_column::is_musical (it))
        continue;

      if (i && !unsmob<Grob> (get_object (cols[i], "left-neighbor")))
        set_object (cols[i], "left-neighbor", cols[i - 1]->self_scm ());
      if (i + 1 < cols.size ()
          && !unsmob<Grob> (get_object (cols[i], "right-neighbor")))
        set_object (cols[i], "right-neighbor", cols[i + 1]->self_scm ());
    }
}
