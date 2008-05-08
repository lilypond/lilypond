/*
  tie-details.hh -- declare Tie_details

  source file of the GNU LilyPond music typesetter

  (c) 2006--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef TIE_DETAILS_HH
#define TIE_DETAILS_HH

#include "lily-proto.hh"

struct Tie_details
{
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


