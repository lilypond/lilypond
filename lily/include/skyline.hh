/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Joe Neeman <joeneeman@gmail.com>

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

#include "lily-proto.hh"
#include "axis.hh"
#include "box.hh"
#include "interval.hh"
#include "direction.hh"
#include "smobs.hh"

#include <vector>

struct Building
{
  Interval x_;
  Real y_intercept_;
  Real slope_;

  void precompute (Real start_height, Real end_height);
  Building (Real start, Real start_height, Real end_height, Real end);
  Building (Box const &b, Axis a, Direction d);
  void print () const;

  Real height (Real x) const;
  Real intersection_x (Building const &other) const;
  bool above (Building const &other, Real x) const;
};

class Skyline : public Simple_smob<Skyline>
{
public:
  static const char *const type_p_name_;

private:
  std::vector<Building> buildings_;
  Direction sky_;

  void internal_merge_skyline (std::vector<Building> const *,
                               std::vector<Building> const *,
                               std::vector<Building> *result) const;
  std::vector<Building> internal_build_skyline (std::vector<Building> *) const;
  Real internal_distance (Skyline const &, Real horizon_padding,
                          Real *touch_point) const;
  Real internal_distance (Skyline const &, Real *touch_point) const;
  void normalize ();

public:
  // FIXME: the argumentless constructor is an attractive nuisance as
  // it makes it easy to forget about setting a direction.  Ideally,
  // we'd get rid of its use in operator[] in std::map<..., Skyline>
  // and remove it.  Changing operator[] to at () requires some care.
  Skyline ();
  Skyline (Direction sky);
  Skyline (std::vector<Box> const &bldgs, Axis a, Direction sky);
  Skyline (std::vector<Drul_array<Offset>> const &bldgs, Axis a, Direction sky);
  Skyline (std::vector<Skyline_pair> const &skypairs, Direction sky);
  Skyline (Box const &b, Axis a, Direction sky);

  Direction sky () { return sky_; }
  void raise (Real);
  void shift (Real);
  void clear ();
  void merge (Skyline const &);
  void insert (Box const &, Axis);
  void set_minimum_height (Real height);

  std::vector<Offset> to_points (Axis) const;
  void print () const;
  void print_points () const;
  Real distance (Skyline const &, Real horizon_padding = 0) const;
  Real touching_point (Skyline const &, Real horizon_padding = 0) const;
  Real height (Real airplane) const;
  Real max_height () const;
  Real max_height_position () const;
  Real left () const;
  Real right () const;
  Direction direction () const;
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
