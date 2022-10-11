/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef Tuplet_number_HH
#define Tuplet_number_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

#include <vector>

struct Tuplet_number
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_x_offset, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_y_offset, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));

  static Grob *select_reference_stem (Spanner *me,
                                      std::vector<Grob *> const &cols);
  static Drul_array<Grob *> adjacent_note_columns (Spanner *me, Grob *ref_stem);
  static bool knee_position_against_beam (Spanner *me, Grob *ref_stem);
};

#endif // Tuplet_number_HH
