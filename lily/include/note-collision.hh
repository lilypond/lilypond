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

#ifndef COLLISION_HH
#define COLLISION_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

#include <vector>

/**
   Resolve conflicts between various Note_columns (chords).

   TODO

   * multistaff support (see Chlapik: equal noteheads should be on the
   same hpos.)

   * Make interface of this, similar to align-interface.
   */
class Note_collision_interface
{
public:
  static SCM automatic_shift (Grob *, Drul_array<std::vector<Grob *>>);
  static SCM forced_shift (Grob *);

  static std::vector<int> note_head_positions (Grob *me);
  static Drul_array<std::vector<Grob *>> get_clash_groups (Grob *me);
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM smob));
  static void add_column (Grob *me, Grob *ncol);
};
#endif // COLLISION_HH
