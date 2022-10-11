/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef BEZIER_HH
#define BEZIER_HH

#include "interval.hh"
#include "ly-scm-list.hh"
#include "offset.hh"
#include "polynomial.hh"

#include <vector>

/**
   Simple bezier curve
*/
class Bezier
{
public:
  Bezier () = default;
  explicit Bezier (const ly_scm_list &control_points);

  void assert_sanity () const;
  void scale (Real x, Real y);
  void reverse ();
  void rotate (Real);
  void translate (Offset);
  void subdivide (Real, Bezier *, Bezier *) const;
  Bezier extract (Real, Real) const;

  Real get_other_coordinate (Axis a, Real x) const;
  std::vector<Real> get_other_coordinates (Axis a, Real x) const;
  std::vector<Real> solve_point (Axis, Real coordinate) const;
  Real minmax (Axis, Real, Real, Direction) const;
  std::vector<Real> solve_derivative (Offset) const;
  Interval extent (Axis) const;
  Interval control_point_extent (Axis) const;

  Polynomial polynomial (Axis) const;
  Offset curve_point (Real t) const;
  Offset dir_at_point (Real t) const;
  Real curve_coordinate (Real t, Axis) const;

  static const int CONTROL_COUNT = 4;

  /*
    Bezier curves always have 4 control points. Making this into an
    std::vector<> gives unnecessary overhead, and makes debugging a royal
    pain.  */

  Offset control_[4];
};

void scale (std::vector<Offset> *array, Real xscale, Real yscale);
void rotate (std::vector<Offset> *array, Real deg);
void translate (std::vector<Offset> *array, Offset o);

Bezier slur_shape (Real width, Real height_limit, Real height_proportion);
Real slur_height (Real width, Real height_limit, Real height_proportion);
void get_slur_indent_height (Real *indent, Real *height, Real width, Real h_inf,
                             Real r_0);

#endif // BEZIER_HH
