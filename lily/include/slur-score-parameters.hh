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

#ifndef SLUR_SCORE_PARAMATERS_HH
#define SLUR_SCORE_PARAMATERS_HH

#include "lily-proto.hh"
#include "real.hh"

struct Slur_score_parameters
{
  int region_size_;

  Real head_encompass_penalty_;
  Real stem_encompass_penalty_;
  Real edge_attraction_factor_;
  Real same_slope_penalty_;
  Real steeper_slope_factor_;
  Real non_horizontal_penalty_;
  Real max_slope_;
  Real max_slope_factor_;
  Real extra_object_collision_penalty_;
  Real accidental_collision_;
  Real free_slur_distance_;
  Real free_head_distance_;
  Real extra_encompass_collision_distance_;
  Real extra_encompass_free_distance_;
  Real gap_to_staffline_inside_;
  Real gap_to_staffline_outside_;
  Real absolute_closeness_measure_;
  Real edge_slope_exponent_;
  Real close_to_edge_length_;
  Real head_slur_distance_max_ratio_;
  Real head_slur_distance_factor_;
  Real encompass_object_range_overshoot_;
  Real slur_tie_extrema_min_distance_;
  Real slur_tie_extrema_min_distance_penalty_;

  void fill (Grob *him);
};
#endif /* SLUR_SCORE_PARAMATERS_HH */
