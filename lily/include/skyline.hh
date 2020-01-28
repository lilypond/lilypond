/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2020 Joe Neeman <joeneeman@gmail.com>

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

#ifndef SKYLINE_HH
#define SKYLINE_HH

#include "axis.hh"
#include "box.hh"
#include "direction.hh"
#include "interval.hh"
#include "lily-proto.hh"
#include "smobs.hh"
#include "std-vector.hh"

#include <list>

struct Building
{
  Real start_;
  Real end_;
  Real y_intercept_;
  Real slope_;

  void precompute (Real start, Real start_height, Real end_height, Real end);
  Building (Real start, Real start_height, Real end_height, Real end);
  Building (Box const &b, Axis a, Direction d);
  void print () const;

  Real height (Real x) const;
  Real intersection_x (Building const &other) const;
  bool above (Building const &other, Real x) const;
  Real shift_to_intersect (Real x, Real y) const;
};

class Skyline : public Simple_smob<Skyline>
{
public:
  static const char *const type_p_name_;

private:
  std::list<Building> buildings_;
  Direction sky_;

  void internal_merge_skyline (std::list<Building> *, std::list<Building> *,
                               std::list<Building> *result) const;
  std::list<Building> internal_build_skyline (std::list<Building> *) const;
  Real internal_distance (Skyline const &, Real horizon_padding,
                          Real *touch_point) const;
  Real internal_distance (Skyline const &, Real *touch_point) const;
  void normalize ();

public:
  Skyline ();
  Skyline (Direction sky);
  Skyline (std::vector<Box> const &bldgs, Axis a, Direction sky);
  Skyline (std::vector<Drul_array<Offset>> const &bldgs, Axis a, Direction sky);
  Skyline (std::vector<Skyline_pair> const &skypairs, Direction sky);
  Skyline (Box const &b, Axis a, Direction sky);

  std::vector<Offset> to_points (Axis) const;
  void merge (Skyline const &);
  void insert (Box const &, Axis);
  void print () const;
  void print_points () const;
  void raise (Real);
  void shift (Real);
  Real distance (Skyline const &, Real horizon_padding = 0) const;
  Real touching_point (Skyline const &, Real horizon_padding = 0) const;
  Real shift_to_avoid (Skyline const &other, Real, Direction d,
                       Real horizon_padding = 0);
  Real raise_to_avoid (Skyline const &other, Real, Direction d,
                       Real horizon_padding = 0);
  Drul_array<Real> shifts_to_avoid_intersection (Skyline const &,
                                                 Real horizon_padding
                                                 = 0) const;
  Interval raises_to_avoid_intersection (Skyline const &,
                                         Real horizon_padding = 0) const;
  Real height (Real airplane) const;
  Real max_height () const;
  Real max_height_position () const;
  Real left () const;
  Real right () const;
  Direction direction () const;
  void set_minimum_height (Real height);
  void clear ();
  bool is_empty () const;
  Skyline padded (Real horizon_padding) const;

  DECLARE_SCHEME_CALLBACK (get_touching_point, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (get_distance, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (get_max_height, (SCM));
  DECLARE_SCHEME_CALLBACK (get_max_height_position, (SCM));
  DECLARE_SCHEME_CALLBACK (get_height, (SCM, SCM));
};

extern bool debug_skylines;

#endif /* SKYLINE_HH */
