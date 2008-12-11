/*
  slur-score-paramaters.cc -- implement Slur_score_parameters

  source file of the GNU LilyPond music typesetter

  (c) 2006--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/


#include "slur-score-parameters.hh"
#include "grob.hh"


Real
get_detail (SCM alist, SCM sym)
{
  SCM entry = scm_assq (sym, alist);
  return robust_scm2double (scm_is_pair (entry)
			    ? scm_cdr (entry)
			    : SCM_EOL,
			    0.0);
}

void
Slur_score_parameters::fill (Grob *me)
{
  SCM details = me->get_property ("details");

  region_size_
    = (int) get_detail (details, ly_symbol2scm ("region-size"));
  head_encompass_penalty_
    = get_detail (details, ly_symbol2scm ("head-encompass-penalty"));
  stem_encompass_penalty_
    = get_detail (details, ly_symbol2scm ("stem-encompass-penalty"));
  closeness_factor_
    = get_detail (details, ly_symbol2scm ("closeness-factor"));
  edge_attraction_factor_
    = get_detail (details, ly_symbol2scm ("edge-attraction-factor"));
  same_slope_penalty_
    = get_detail (details, ly_symbol2scm ("same-slope-penalty"));
  steeper_slope_factor_
    = get_detail (details, ly_symbol2scm ("steeper-slope-factor"));
  non_horizontal_penalty_
    = get_detail (details, ly_symbol2scm ("non-horizontal-penalty"));
  max_slope_
    = get_detail (details, ly_symbol2scm ("max-slope"));
  max_slope_factor_
    = get_detail (details, ly_symbol2scm ("max-slope-factor"));
  free_head_distance_
    = get_detail (details, ly_symbol2scm ("free-head-distance"));
  absolute_closeness_measure_
    = get_detail (details, ly_symbol2scm ("absolute-closeness-measure"));
  extra_object_collision_penalty_
    = get_detail (details, ly_symbol2scm ("extra-object-collision-penalty"));
  accidental_collision_
    = get_detail (details, ly_symbol2scm ("accidental-collision"));
  extra_encompass_free_distance_
    = get_detail (details, ly_symbol2scm ("extra-encompass-free-distance"));
  extra_encompass_collision_distance_
    = get_detail (details, ly_symbol2scm ("extra-encompass-collision-distance"));
  head_slur_distance_factor_
    = get_detail (details, ly_symbol2scm ("head-slur-distance-factor"));
  head_slur_distance_max_ratio_
    = get_detail (details, ly_symbol2scm ("head-slur-distance-max-ratio"));
  free_slur_distance_
    = get_detail (details, ly_symbol2scm ("free-slur-distance"));
  edge_slope_exponent_
    = get_detail (details, ly_symbol2scm ("edge-slope-exponent"));
}
