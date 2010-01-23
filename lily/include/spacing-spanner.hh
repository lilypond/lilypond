/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SPACING_SPANNER_HH
#define SPACING_SPANNER_HH

#include "lily-proto.hh"
#include "rational.hh"
#include "std-vector.hh"
#include "grob-interface.hh"
#include "spring.hh"

class Spacing_spanner
{
private:
  static void set_distances_for_loose_col (Grob *me, Grob *c, Drul_array<Item *> next_door, Spacing_options const *);
  static void generate_pair_spacing (Grob *me,
				     Paper_column *l, Paper_column *r,
				     Paper_column *nextr,
				     Spacing_options const *options);
  static Real default_bar_spacing (Grob *, Grob *, Grob *, Moment);
  static Rational effective_shortest_duration (Grob *me, vector<Grob*> const &all);
  static void breakable_column_spacing (Grob *, Item *l, Item *r, Spacing_options const *);
  static void prune_loose_columns (Grob *, vector<Grob*> *cols, Spacing_options  *);
  static void set_explicit_neighbor_columns (vector<Grob*> const &cols);
  static void set_implicit_neighbor_columns (vector<Grob*> const &cols);
  static void generate_springs (Grob *me, vector<Grob*> const &cols, Spacing_options const *);
  static void musical_column_spacing (Grob *, Item *, Item *, Spacing_options const *);
  static bool fills_measure (Grob *, Item *, Item *);
public:
  static vector<Grob*> get_columns (Grob *me);
  static Real note_spacing (Grob *, Grob *, Grob *, Spacing_options const *);
  static Spring standard_breakable_column_spacing (Grob *me, Item *l, Item *r, Spacing_options const *);
  
  DECLARE_SCHEME_CALLBACK (set_springs, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_common_shortest_duration, (SCM));
  DECLARE_GROB_INTERFACE();
};

#endif /* SPACING_SPANNER_HH */
