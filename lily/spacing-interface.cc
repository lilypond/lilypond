/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Joe Neeman <joeneeman@gmail.com>

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

#include "spacing-interface.hh"

#include "grob.hh"
#include "grob-array.hh"
#include "item.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "paper-column.hh"
#include "separation-item.hh"
#include "skyline.hh"
#include "skyline-pair.hh"
#include "system.hh"

using std::vector;

/* return the right-pointing skyline of the left-items and the left-pointing
   skyline of the right-items (with the skyline of the left-items in
   ret[LEFT]) */
Drul_array<Skyline>
Spacing_interface::skylines (Grob *me, Grob *right_col)
{
  /* the logic here is a little convoluted.
     A {Staff,Note}_spacing doesn't copy left-items when it clones,
     so in order to find the separation items, we need to use the original
     spacing grob. But once we find the separation items, we need to get back
     the broken piece.
  */

  Grob *orig = me->original () ? me->original () : me;
  Drul_array<Direction> break_dirs (
    dynamic_cast<Item *> (me)->break_status_dir (),
    dynamic_cast<Item *> (right_col)->break_status_dir ());
  Drul_array<Skyline> skylines
    = Drul_array<Skyline> (Skyline (RIGHT), Skyline (LEFT));
  Drul_array<vector<Grob *>> items (
    ly_scm2link_array (get_object (orig, "left-items")),
    ly_scm2link_array (get_object (orig, "right-items")));

  Grob *system = me->get_system ();
  Grob *left_col = dynamic_cast<Item *> (me)->get_column ();

  Drul_array<Grob *> columns (left_col, right_col);

  for (const auto d : {LEFT, RIGHT})
    {
      for (vsize i = 0; i < items[d].size (); i++)
        {
          Item *g = dynamic_cast<Item *> (items[d][i]);
          if (g)
            if (Item *piece = g->find_prebroken_piece (break_dirs[d]))
              g = piece;

          if (has_interface<Separation_item> (g)
              && g->get_column () == columns[d])
            {
              SCM skyp_scm = get_property (g, "horizontal-skylines");
              if (!is_scm<Skyline_pair> (skyp_scm))
                programming_error ("separation item has no skyline");
              const Skyline_pair &skyp = from_scm<Skyline_pair> (skyp_scm);

              extract_grob_set (g, "elements", elts);
              Grob *ycommon = common_refpoint_of_array (elts, g, Y_AXIS);
              Real shift
                = ycommon->pure_relative_y_coordinate (system, 0, INT_MAX);

              skylines[d].shift (-shift);

              skylines[d].merge (skyp[-d]);

              if (d == RIGHT && items[LEFT].size ())
                skylines[d].merge (Separation_item::conditional_skyline (
                  items[d][i], items[LEFT][0]));

              skylines[d].shift (shift);
            }
        }
    }

  return skylines;
}

Real
Spacing_interface::minimum_distance (Grob *me, Grob *right)
{
  Drul_array<Skyline> skylines = Spacing_interface::skylines (me, right);

  return std::max (0.0, skylines[LEFT].distance (skylines[RIGHT]));
}

/*
  Compute the left-most column of the right-items.
*/
Paper_column *
Spacing_interface::right_column (Grob *me)
{
  if (!me->is_live ())
    return 0;

  Paper_column *mincol = 0;
  if (Grob_array *a = unsmob<Grob_array> (get_object (me, "right-items")))
    {
      int min_rank = INT_MAX;
      for (vsize i = 0; i < a->size (); i++)
        {
          if (Item *ri = dynamic_cast<Item *> (a->grob (i)))
            {
              if (Paper_column *col = ri->get_column ())
                {
                  int rank = col->get_rank ();
                  if (rank < min_rank)
                    {
                      min_rank = rank;
                      mincol = col;
                    }
                }
            }
        }
    }

  return mincol;
}

Paper_column *
Spacing_interface::left_column (Grob *me_as_grob)
{
  Item *me = dynamic_cast<Item *> (me_as_grob);
  if (!me || !me->is_live ())
    return 0;

  return me->get_column ();
}

static vector<Item *>
get_note_columns (vector<Grob *> const &elts)
{
  vector<Item *> ret;

  for (vsize i = 0; i < elts.size (); i++)
    {
      if (has_interface<Note_column> (elts[i]))
        ret.push_back (dynamic_cast<Item *> (elts[i]));
      else if (has_interface<Separation_item> (elts[i]))
        {
          extract_grob_set (elts[i], "elements", more_elts);
          vector<Item *> ncs = get_note_columns (more_elts);

          ret.insert (ret.end (), ncs.begin (), ncs.end ());
        }
    }

  return ret;
}

vector<Item *>
Spacing_interface::right_note_columns (Grob *me)
{
  extract_grob_set (me, "right-items", elts);
  return get_note_columns (elts);
}

vector<Item *>
Spacing_interface::left_note_columns (Grob *me)
{
  extract_grob_set (me, "left-items", elts);
  return get_note_columns (elts);
}

/*
  Try to find the break-aligned symbol that belongs on the D-side
  of ME, sticking out in direction -D. The x size is put in LAST_EXT
*/
Grob *
Spacing_interface::extremal_break_aligned_grob (Grob *me, Direction d,
                                                Direction break_dir,
                                                Interval *last_ext)
{
  Grob *col = 0;
  last_ext->set_empty ();
  Grob *last_grob = 0;

  extract_grob_set (
    me, d == LEFT ? "left-break-aligned" : "right-break-aligned", elts);

  for (vsize i = elts.size (); i--;)
    {
      Item *break_item = dynamic_cast<Item *> (elts[i]);

      if (break_item->break_status_dir () != break_dir)
        break_item = break_item->find_prebroken_piece (break_dir);

      if (!break_item
          || !scm_is_pair (get_property (break_item, "space-alist")))
        continue;

      if (!col)
        col = dynamic_cast<Item *> (elts[0])
                ->get_column ()
                ->find_prebroken_piece (break_dir);

      Interval ext = break_item->extent (col, X_AXIS);

      if (ext.is_empty ())
        continue;

      if (!last_grob || (last_grob && d * (ext[-d] - (*last_ext)[-d]) < 0))
        {
          *last_ext = ext;
          last_grob = break_item;
        }
    }

  return last_grob;
}

ADD_INTERFACE (Spacing_interface,
               R"(
This object calculates the desired and minimum distances between two columns.
               )",

               /* properties */
               R"(
left-items
right-items
               )");
