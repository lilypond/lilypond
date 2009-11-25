/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "std-vector.hh"
#include "grob-interface.hh"

class Tuplet_bracket
{
public:
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positions, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_control_points, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_connect_to_neighbors, (SCM smob));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  
  DECLARE_GROB_INTERFACE();
  static Grob* get_common_x (Spanner *);
  static void add_tuplet_bracket (Grob *me, Grob *smaller_bracket);
  static void get_bounds (Grob *, Grob **, Grob **);
  static void add_column (Grob *me, Item *);
  static void add_beam (Grob *me, Grob *);
  static Grob *parallel_beam (Grob *me, vector<Grob*> const &cols,
			      bool *equally_long);
  static void calc_position_and_height (Grob *, Real *, Real *dy);
  static Stencil make_bracket (Grob *me, Axis protrusion_axis,
			       Offset dz, Drul_array<Real> height,
			       Interval gap, Drul_array<Real> widen,
			       Drul_array<Real> shorten);
  static Direction get_default_dir (Grob *);
};

#endif // Tuplet_bracket_HH

