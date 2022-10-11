/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "tie.hh"
#include "bezier.hh"
#include "grob.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"
#include "tie-details.hh"

/*
  this is a macro because we want ly_symbol2scm()
 */
#define get_real_detail(src, defvalue)                                         \
  from_scm<double> (ly_assoc_get (ly_symbol2scm (src), details, SCM_EOL),      \
                    defvalue)
#define get_int_detail(src, defvalue)                                          \
  from_scm (ly_assoc_get (ly_symbol2scm (src), details, SCM_EOL), defvalue)

void
Tie_details::from_grob (Grob *me)
{
  staff_symbol_referencer_ = me;
  staff_space_ = Staff_symbol_referencer::staff_space (me);

  neutral_direction_
    = from_scm<Direction> (get_property (me, "neutral-direction"));
  if (!neutral_direction_)
    neutral_direction_ = DOWN;

  SCM details = get_property (me, "details");

  height_limit_ = get_real_detail ("height-limit", 0.75);
  ratio_ = get_real_detail ("ratio", .333);
  between_length_limit_ = get_real_detail ("between-length-limit", 1.0);

  wrong_direction_offset_penalty_
    = get_real_detail ("wrong-direction-offset-penalty", 10);

  min_length_ = get_real_detail ("min-length", 1.0);
  min_length_penalty_factor_
    = get_real_detail ("min-length-penalty-factor", 1.0);

  // in half-space
  center_staff_line_clearance_
    = get_real_detail ("center-staff-line-clearance", 0.4);
  tip_staff_line_clearance_ = get_real_detail ("tip-staff-line-clearance", 0.4);
  staff_line_collision_penalty_
    = get_real_detail ("staff-line-collision-penalty", 5);
  dot_collision_clearance_ = get_real_detail ("dot-collision-clearance", 0.25);
  dot_collision_penalty_ = get_real_detail ("dot-collision-penalty", 0.25);
  x_gap_ = get_real_detail ("note-head-gap", 0.2);
  stem_gap_ = get_real_detail ("stem-gap", 0.3);
  tie_column_monotonicity_penalty_
    = get_real_detail ("tie-column-monotonicity-penalty", 100);
  tie_tie_collision_penalty_
    = get_real_detail ("tie-tie-collision-penalty", 30);
  tie_tie_collision_distance_
    = get_real_detail ("tie-tie-collision-distance", .25);
  horizontal_distance_penalty_factor_
    = get_real_detail ("horizontal-distance-penalty-factor", 5);
  same_dir_as_stem_penalty_ = get_real_detail ("same-dir-as-stem-penalty", 20);
  vertical_distance_penalty_factor_
    = get_real_detail ("vertical-distance-penalty-factor", 5);
  intra_space_threshold_ = get_real_detail ("intra-space-threshold", 1.0);
  outer_tie_length_symmetry_penalty_factor_
    = get_real_detail ("outer-tie-length-symmetry-penalty-factor", 3.0);
  outer_tie_vertical_distance_symmetry_penalty_factor_ = get_real_detail (
    "outer-tie-vertical-distance-symmetry-penalty-factor", 3.0);

  outer_tie_vertical_gap_ = get_real_detail ("outer-tie-vertical-gap", 0.15);

  single_tie_region_size_ = get_int_detail ("single-tie-region-size", 3);
  skyline_padding_ = get_real_detail ("skyline-padding", 0.05);
  multi_tie_region_size_ = get_int_detail ("multi-tie-region-size", 1);
}

Tie_details::Tie_details ()
{
  staff_space_ = 1.0;
  height_limit_ = 1.0;
  ratio_ = .333;
}
