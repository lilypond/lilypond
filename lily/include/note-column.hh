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

#ifndef NOTE_COLUMN_HH
#define NOTE_COLUMN_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

/** a struct for treating a group of noteheads (noteheads, stem
    (chord) and scripts) as a single entity.

    UGR. Junkme.  refpoint should be the notehead, dir should come from stem.
*/
class Note_column
{
public:
  static bool shift_less (Grob *const &, Grob *const &);
  static Direction dir (Grob *me);
  static Grob *accidentals (Grob *me);
  static Slice head_positions_interval (Grob *me);
  static Grob *first_head (Grob *me);
  static Drul_array<Grob *> extremal_heads (Grob *me);
  static Grob *get_rest (Grob *me);
  static void set_stem (Grob *me, Grob *);
  static void add_head (Grob *me, Grob *);
  static bool has_rests (Grob *me);
  static Grob *dot_column (Grob *me);
  static Interval cross_staff_extent (Grob *me, Grob *refp);

  static Item *get_stem (Grob *);
  static Item *get_flag (Grob *);

  DECLARE_SCHEME_CALLBACK (calc_main_extent, (SCM));
};

#endif // NOTE_COLUMN_HH
