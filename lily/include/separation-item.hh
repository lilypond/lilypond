/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SINGLE_MALT_GROUPING_ITEM_HH
#define SINGLE_MALT_GROUPING_ITEM_HH

#include "lily-proto.hh"
#include "direction.hh"
#include "grob-interface.hh"
#include "skyline.hh"

#include <vector>

struct Separation_item
{
  DECLARE_SCHEME_CALLBACK (calc_skylines, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));

  static std::vector<Box> boxes (Grob *me, Grob *left);
  static Skyline conditional_skyline (Grob *, Grob *);
  static Grob *extremal_break_aligned_grob (Grob *, Direction, Interval *);
  static Real set_distance (Item *left, Item *right, Real padding);
  static bool is_empty (Grob *me);
  static void add_item (Grob *, Item *);
  static void add_conditional_item (Grob *, Grob *);
};

#endif /* SINGLE_MALT_GROUPING_ITEM_HH */
