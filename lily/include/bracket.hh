/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef BRACKET_HH
#define BRACKET_HH

#include "grob-interface.hh"
#include "lily-proto.hh"
#include "std-vector.hh"

struct Bracket
{
  static Stencil make_bracket (Grob *me, Axis protrusion_axis, Offset dz,
                               Drul_array<Real> height, Interval gap,
                               Drul_array<Real> flare,
                               Drul_array<Real> shorten);
  static Stencil make_axis_constrained_bracket (Grob *me, Real length, Axis a,
                                                Direction dir, Interval gap);
  static Stencil make_enclosing_bracket (Grob *me, Grob *refpoint,
                                         std::vector<Grob *> grobs, Axis a,
                                         Direction dir);
};

#endif /* BRACKET_HH */
