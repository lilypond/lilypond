/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef LOOKUP_HH
#define LOOKUP_HH

#include "stencil.hh"

#include <vector>

namespace Lookup
{
Stencil bracket (Axis a, Interval iv, Real thick, Real protrude, Real blot);
Stencil circle (Real rad, Real thick, bool filled);
Stencil rotated_box (Real slope, Real width, Real thick, Real blot);
Stencil round_polygon (std::vector<Offset> const &points, Real blotdiameter,
                       Real extroversion = 0, bool filled = true);
Stencil frame (Box b, Real thick, Real blot);
Stencil slur (Bezier controls, Real cthick, Real thick, SCM dash_definition);
Stencil bezier_sandwich (Bezier top_curve, Bezier bottom_curve, Real thickness);
Stencil beam (Real slope, Real width, Real thick, Real blot);
Stencil blank (Box b);
Stencil filled_box (Box b);
Stencil round_filled_box (Box b, Real blotdiameter);
Stencil repeat_slash (Real w, Real slope, Real th);
Stencil horizontal_line (Interval w, Real th);
Stencil triangle (Interval iv, Real thick, Real protrude);
Stencil points_to_line_stencil (Real thick, std::vector<Offset> const &points);
}; // namespace Lookup

#endif // LOOKUP_HH
