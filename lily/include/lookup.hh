/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
#include "std-vector.hh"

struct Lookup
{
  static Stencil dot (Offset p, Real radius);
  static Stencil bracket (Axis a, Interval iv, Real thick, Real protrude, Real blot);
  static Stencil rotated_box (Real slope, Real width, Real thick, Real blot);
  static Stencil round_filled_polygon (vector<Offset> const &points, Real blotdiameter);
  static Stencil frame (Box b, Real thick, Real blot);
  static Stencil slur (Bezier controls, Real cthick, Real thick, 
                       SCM dash_definition);
  static Stencil bezier_sandwich (Bezier top_curve, Bezier bottom_curve,
                                  Real thickness);
  static Stencil beam (Real slope, Real width, Real thick, Real blot);
  static Stencil blank (Box b);
  static Stencil filled_box (Box b);
  static Stencil round_filled_box (Box b, Real blotdiameter);
  static Stencil repeat_slash (Real w, Real slope, Real th);
  static Stencil horizontal_line (Interval w, Real th);
  static Stencil triangle (Interval iv, Real thick, Real protrude);
  static Stencil points_to_line_stencil (Real thick, vector<Offset> const &points);
};

#endif // LOOKUP_HH
