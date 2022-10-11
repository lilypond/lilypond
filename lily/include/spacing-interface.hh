/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SPACING_INTERFACE_HH
#define SPACING_INTERFACE_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

#include <vector>

struct Spacing_interface
{
  static Real minimum_distance (Grob *me, Grob *right_col);
  static std::vector<Item *> right_note_columns (Grob *me);
  static std::vector<Item *> left_note_columns (Grob *me);
  static Paper_column *right_column (Grob *me);
  static Paper_column *left_column (Grob *me);
  static Drul_array<Skyline> skylines (Grob *me, Grob *right_col);
  static Grob *extremal_break_aligned_grob (Grob *me, Direction, Direction,
                                            Interval *);
};

#endif /* SPACING_INTERFACE_HH */
