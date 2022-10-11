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

#ifndef Tuplet_bracket_HH
#define Tuplet_bracket_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

#include <vector>

class Tuplet_bracket
{
public:
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positions, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_x_positions, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_connect_to_neighbors, (SCM smob));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));

  static Grob *get_common_x (Spanner *);
  static void add_tuplet_bracket (Grob *me, Grob *smaller_bracket);
  static void get_bounds (Grob *, Grob **, Grob **);
  static void add_column (Spanner *me, Item *);
  static void add_script (Grob *me, Item *);
  static void add_beam (Grob *me, Grob *);
  static Spanner *parallel_beam (Spanner *me, std::vector<Grob *> const &cols);
  static void calc_position_and_height (Spanner *, Real *, Real *dy);
  static Direction get_default_dir (Grob *);
};

#endif // Tuplet_bracket_HH
