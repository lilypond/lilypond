/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "hara-kiri-group-spanner.hh"

#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "axis-group-interface.hh"
#include "spanner.hh"
#include "warn.hh"

#include <algorithm>
#include <vector>

using std::vector;

MAKE_SCHEME_CALLBACK (Hara_kiri_group_spanner, y_extent,
                      "ly:hara-kiri-group-spanner::y-extent", 1);
SCM
Hara_kiri_group_spanner::y_extent (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  consider_suicide (me);
  return Axis_group_interface::generic_group_extent (me, Y_AXIS);
}

MAKE_SCHEME_CALLBACK (Hara_kiri_group_spanner, calc_skylines,
                      "ly:hara-kiri-group-spanner::calc-skylines", 1);
SCM
Hara_kiri_group_spanner::calc_skylines (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  consider_suicide (me);
  return Axis_group_interface::calc_skylines (smob);
}

MAKE_SCHEME_CALLBACK (Hara_kiri_group_spanner, pure_height,
                      "ly:hara-kiri-group-spanner::pure-height", 3);
SCM
Hara_kiri_group_spanner::pure_height (SCM smob, SCM start_scm, SCM end_scm)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  int start = from_scm (start_scm, 0);
  int end = from_scm (end_scm, INT_MAX);

  if (request_suicide (me, start, end))
    return to_scm (Interval ());

  return to_scm (Axis_group_interface::pure_group_height (me, start, end));
}

/* there is probably a way that doesn't involve re-implementing a binary
   search (I would love some proper closures right now) */
bool
find_in_range (SCM vector, vsize low, vsize hi, vsize min, vsize max)
{
  if (low >= hi)
    return false;

  vsize mid = low + (hi - low) / 2;
  vsize val = scm_to_size_t (scm_c_vector_ref (vector, mid));
  if (val >= min && val <= max)
    return true;
  else if (val < min)
    return find_in_range (vector, mid + 1, hi, min, max);
  return find_in_range (vector, low, mid, min, max);
}

bool
Hara_kiri_group_spanner::request_suicide (Grob *me, vsize start, vsize end)
{
  extract_grob_set (me, "make-dead-when", foes);
  for (vsize i = 0; i < foes.size (); i++)
    if (foes[i]->is_live () && !request_suicide_alone (foes[i], start, end))
      return true;

  if (!request_suicide_alone (me, start, end))
    return false;

  extract_grob_set (me, "keep-alive-with", friends);
  for (vsize i = 0; i < friends.size (); ++i)
    if (friends[i]->is_live ()
        && !request_suicide_alone (friends[i], start, end))
      return false;

  return true;
}

bool
Hara_kiri_group_spanner::request_suicide_alone (Grob *me, vsize start,
                                                vsize end)
{
  if (!from_scm<bool> (get_property (me, "remove-empty")))
    return false;

  bool remove_first = from_scm<bool> (get_property (me, "remove-first"));
  if (!remove_first && start <= 0)
    return false;

  SCM important = get_property (me, "important-column-ranks");
  if (scm_is_vector (important))
    {
      vsize len = scm_c_vector_length (important);
      if (find_in_range (important, 0, len, start, end))
        return false;
    }
  else /* build the important-columns-cache */
    {
      extract_grob_set (me, "items-worth-living", worth);
      vector<int> ranks;

      for (vsize i = 0; i < worth.size (); i++)
        {
          Interval_t<int> iv = worth[i]->spanned_column_rank_interval ();
          for (int j = iv[LEFT]; j <= iv[RIGHT]; j++)
            ranks.push_back (j);
        }
      std::sort (ranks.begin (), ranks.end ());
      auto last = std::unique (ranks.begin (), ranks.end ());
      vsize vector_size = last - ranks.begin ();

      SCM scm_vec = scm_c_make_vector (vector_size, SCM_EOL);
      for (vsize i = 0; i < vector_size; i++)
        scm_c_vector_set_x (scm_vec, i, to_scm (ranks[i]));
      set_property (me, "important-column-ranks", scm_vec);

      return request_suicide (me, start, end);
    }

  return true;
}

void
Hara_kiri_group_spanner::consider_suicide (Grob *me)
{
  Spanner *sp = dynamic_cast<Spanner *> (me);
  int left = 0;
  int right = INT_MAX;
  if (Item *l = sp->get_bound (LEFT))
    left = l->get_column ()->get_rank ();
  if (Item *r = sp->get_bound (RIGHT))
    right = r->get_column ()->get_rank ();
  if (!request_suicide (me, left, right))
    return;

  vector<Grob *> childs;
  Axis_group_interface::get_children (me, &childs);
  for (vsize i = 0; i < childs.size (); i++)
    childs[i]->suicide ();

  /*
    very appropriate name here :-)
  */
  me->suicide ();
}

/*
  We can't rely on offsets and dimensions of elements in a hara-kiri
  group. Use a callback to make sure that hara-kiri has been done
  before asking for offsets.  */
MAKE_SCHEME_CALLBACK (Hara_kiri_group_spanner, force_hara_kiri_callback,
                      "ly:hara-kiri-group-spanner::force-hara-kiri-callback",
                      1);
SCM
Hara_kiri_group_spanner::force_hara_kiri_callback (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  consider_suicide (me);
  return to_scm (0.0);
}

MAKE_SCHEME_CALLBACK (
  Hara_kiri_group_spanner, force_hara_kiri_in_y_parent_callback,
  "ly:hara-kiri-group-spanner::force-hara-kiri-in-y-parent-callback", 1);
SCM
Hara_kiri_group_spanner::force_hara_kiri_in_y_parent_callback (SCM smob)
{
  auto *const daughter = LY_ASSERT_SMOB (Grob, smob, 1);
  force_hara_kiri_callback (daughter->get_y_parent ()->self_scm ());
  return to_scm (0.0);
}

void
Hara_kiri_group_spanner::add_interesting_item (Grob *me, Grob *n)
{
  Pointer_group_interface::add_unordered_grob (
    me, ly_symbol2scm ("items-worth-living"), n);
}

ADD_INTERFACE (Hara_kiri_group_spanner,
               R"(
A group spanner that keeps track of interesting items.  If it doesn't contain
any after line breaking, it removes itself and all its children.  Greater
control can be exercised via @code{remove-layer} which can prioritize layers so
only the lowest-numbered non-empty layer is retained; make the layer
independent of the group; or make it dependent on any other member of the group
               )",

               /* properties */
               R"(
items-worth-living
important-column-ranks
keep-alive-with
make-dead-when
remove-empty
remove-first
remove-layer
               )");
