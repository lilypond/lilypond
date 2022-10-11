/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2022 Mike Solomon <mike@mikesolomon.org>

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
#include "item.hh"
#include "pointer-group-interface.hh"
#include "pure-from-neighbor-interface.hh"
#include "engraver.hh"

#include "translator.icc"

#include <algorithm>
#include <array>
#include <map>
#include <vector>

using std::vector;

class Pure_from_neighbor_engraver : public Engraver
{
  vector<Item *> pure_relevants_;
  vector<Item *> need_pure_heights_from_neighbors_;

public:
  TRANSLATOR_DECLARATIONS (Pure_from_neighbor_engraver);

protected:
  void acknowledge_item (Grob_info_t<Item>);
  void finalize () override;
};

Pure_from_neighbor_engraver::Pure_from_neighbor_engraver (Context *c)
  : Engraver (c)
{
}

void
Pure_from_neighbor_engraver::acknowledge_item (Grob_info_t<Item> info)
{
  auto *const item = info.grob ();
  if (has_interface<Pure_from_neighbor_interface> (item))
    need_pure_heights_from_neighbors_.push_back (item);
  else
    pure_relevants_.push_back (item);
}

bool
in_same_column (Grob *g1, Grob *g2)
{
  return (g1->spanned_column_rank_interval ()[LEFT]
          == g2->spanned_column_rank_interval ()[LEFT])
         && (g1->spanned_column_rank_interval ()[RIGHT]
             == g2->spanned_column_rank_interval ()[RIGHT])
         && (g1->spanned_column_rank_interval ()[LEFT]
             == g1->spanned_column_rank_interval ()[RIGHT]);
}

void
Pure_from_neighbor_engraver::finalize ()
{
  if (!need_pure_heights_from_neighbors_.size ())
    return;

  std::sort (need_pure_heights_from_neighbors_.begin (),
             need_pure_heights_from_neighbors_.end (), Grob::less);
  std::sort (pure_relevants_.begin (), pure_relevants_.end (), Grob::less);

  /*
    first, clump need_pure_heights_from_neighbors into
    vectors of grobs that have the same column.
  */

  vsize l = 0;
  vector<vector<Grob *>> need_pure_heights_from_neighbors;
  do
    {
      vector<Grob *> temp;
      temp.push_back (need_pure_heights_from_neighbors_[l]);
      for (; (l < need_pure_heights_from_neighbors_.size () - 1
              && ((need_pure_heights_from_neighbors_[l]
                     ->spanned_column_rank_interval ()[LEFT])
                  == (need_pure_heights_from_neighbors_[l + 1]
                        ->spanned_column_rank_interval ()[LEFT])));
           l++)
        temp.push_back (need_pure_heights_from_neighbors_[l + 1]);
      need_pure_heights_from_neighbors.push_back (temp);
      l++;
    }
  while (l < need_pure_heights_from_neighbors_.size ());

  /*
    then, loop through the pure_relevants_ list, adding the items
    to the elements of need_pure_heights_from_neighbors_ on either side.
  */

  std::array<vsize, 2> pos {VPOS, 0};
  for (const auto &pure_relevant : pure_relevants_)
    {
      while (pos[1] < need_pure_heights_from_neighbors.size ()
             && (pure_relevant->spanned_column_rank_interval ()[LEFT]
                 > (need_pure_heights_from_neighbors[pos[1]][0]
                      ->spanned_column_rank_interval ()[LEFT])))
        {
          pos[0] = pos[1];
          pos[1]++;
        }

      for (const auto p : pos)
        {
          if ((p != VPOS) && (p < need_pure_heights_from_neighbors.size ()))
            {
              for (vsize k = 0; k < need_pure_heights_from_neighbors[p].size ();
                   k++)
                {
                  if (!in_same_column (need_pure_heights_from_neighbors[p][k],
                                       pure_relevant))
                    Pointer_group_interface::add_grob (
                      need_pure_heights_from_neighbors[p][k],
                      ly_symbol2scm ("neighbors"), pure_relevant);
                }
            }
        }
    }

  need_pure_heights_from_neighbors_.clear ();
  pure_relevants_.clear ();
}

void
Pure_from_neighbor_engraver::boot ()
{
  ADD_ACKNOWLEDGER (item);
}

ADD_TRANSLATOR (Pure_from_neighbor_engraver,
                /* doc */
                R"(
Coordinates items that get their pure heights from their neighbors.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
