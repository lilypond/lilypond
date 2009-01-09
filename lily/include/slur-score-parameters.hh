/*
  slur-score-paramaters.hh -- declare Slur_score_parameters

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef SLUR_SCORE_PARAMATERS_HH
#define SLUR_SCORE_PARAMATERS_HH

#include "lily-proto.hh"

struct Slur_score_parameters
{
  int region_size_;

  Real head_encompass_penalty_;
  Real stem_encompass_penalty_;
  Real closeness_factor_;
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
  Real absolute_closeness_measure_;
  Real edge_slope_exponent_;
  Real head_slur_distance_max_ratio_;
  Real head_slur_distance_factor_;

  void fill (Grob *him);
};
#endif /* SLUR_SCORE_PARAMATERS_HH */
