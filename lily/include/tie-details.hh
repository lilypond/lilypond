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

#ifndef TIE_DETAILS_HH
#define TIE_DETAILS_HH

#include "lily-proto.hh"

class Tie_details
{
public:
  Real height_limit_;
  Real ratio_;
  Real staff_space_;
  Real x_gap_;
  Real stem_gap_;
  Real between_length_limit_;
  Real wrong_direction_offset_penalty_;
  Real same_dir_as_stem_penalty_;
  Real min_length_penalty_factor_;
  Real min_length_;
  Real skyline_padding_;
  Real tip_staff_line_clearance_;
  Real center_staff_line_clearance_;
  Real staff_line_collision_penalty_;
  Real dot_collision_clearance_;
  Real dot_collision_penalty_;
  Real tie_column_monotonicity_penalty_;
  Real tie_tie_collision_penalty_;
  Real tie_tie_collision_distance_;
  Real horizontal_distance_penalty_factor_;
  Real vertical_distance_penalty_factor_;
  Real intra_space_threshold_;
  Real outer_tie_length_symmetry_penalty_factor_;
  Real outer_tie_vertical_distance_symmetry_penalty_factor_;
  Real outer_tie_vertical_gap_;
  Grob *staff_symbol_referencer_;

  int single_tie_region_size_;
  int multi_tie_region_size_;
  Direction neutral_direction_;

  Tie_details ();
  void from_grob (Grob *);
};

#endif /* TIE_DETAILS_HH */
