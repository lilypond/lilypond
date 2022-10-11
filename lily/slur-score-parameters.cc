/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "slur-score-parameters.hh"
#include "grob.hh"

Real
get_detail (SCM alist, SCM sym)
{
  SCM entry = scm_assq (sym, alist);
  return from_scm<double> (scm_is_pair (entry) ? scm_cdr (entry) : SCM_EOL,
                           0.0);
}

void
Slur_score_parameters::fill (Grob *me)
{
  SCM details = get_property (me, "details");

  region_size_
    = static_cast<int> (get_detail (details, ly_symbol2scm ("region-size")));
  head_encompass_penalty_
    = get_detail (details, ly_symbol2scm ("head-encompass-penalty"));
  stem_encompass_penalty_
    = get_detail (details, ly_symbol2scm ("stem-encompass-penalty"));
  edge_attraction_factor_
    = get_detail (details, ly_symbol2scm ("edge-attraction-factor"));
  same_slope_penalty_
    = get_detail (details, ly_symbol2scm ("same-slope-penalty"));
  steeper_slope_factor_
    = get_detail (details, ly_symbol2scm ("steeper-slope-factor"));
  non_horizontal_penalty_
    = get_detail (details, ly_symbol2scm ("non-horizontal-penalty"));
  max_slope_ = get_detail (details, ly_symbol2scm ("max-slope"));
  max_slope_factor_ = get_detail (details, ly_symbol2scm ("max-slope-factor"));
  free_head_distance_
    = get_detail (details, ly_symbol2scm ("free-head-distance"));
  gap_to_staffline_inside_
    = get_detail (details, ly_symbol2scm ("gap-to-staffline-inside"));
  gap_to_staffline_outside_
    = get_detail (details, ly_symbol2scm ("gap-to-staffline-outside"));
  absolute_closeness_measure_
    = get_detail (details, ly_symbol2scm ("absolute-closeness-measure"));
  extra_object_collision_penalty_
    = get_detail (details, ly_symbol2scm ("extra-object-collision-penalty"));
  accidental_collision_
    = get_detail (details, ly_symbol2scm ("accidental-collision"));
  extra_encompass_free_distance_
    = get_detail (details, ly_symbol2scm ("extra-encompass-free-distance"));
  extra_encompass_collision_distance_ = get_detail (
    details, ly_symbol2scm ("extra-encompass-collision-distance"));
  head_slur_distance_factor_
    = get_detail (details, ly_symbol2scm ("head-slur-distance-factor"));
  head_slur_distance_max_ratio_
    = get_detail (details, ly_symbol2scm ("head-slur-distance-max-ratio"));
  free_slur_distance_
    = get_detail (details, ly_symbol2scm ("free-slur-distance"));
  edge_slope_exponent_
    = get_detail (details, ly_symbol2scm ("edge-slope-exponent"));
  close_to_edge_length_
    = get_detail (details, ly_symbol2scm ("close-to-edge-length"));
  encompass_object_range_overshoot_
    = get_detail (details, ly_symbol2scm ("encompass-object-range-overshoot"));
  slur_tie_extrema_min_distance_
    = get_detail (details, ly_symbol2scm ("slur-tie-extrema-min-distance"));
  slur_tie_extrema_min_distance_penalty_ = get_detail (
    details, ly_symbol2scm ("slur-tie-extrema-min-distance-penalty"));
}
