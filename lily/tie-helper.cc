/*
  tie-helper.cc -- implement Tie_configuration, Tie_details

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "tie.hh"
#include "bezier.hh"
#include "grob.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"
#include "tie-formatting-problem.hh"


#define get_real_detail(src, defvalue) robust_scm2double(ly_assoc_get (ly_symbol2scm (src), details, SCM_EOL), defvalue)

void
Tie_details::from_grob (Grob *me)
{
  staff_symbol_referencer_ = me;
  staff_space_ = Staff_symbol_referencer::staff_space (me);
  x_gap_ = robust_scm2double (me->get_property ("x-gap"), 0.2);

  SCM details = me->get_property ("details");

  height_limit_ = get_real_detail("height-limit", 0.75);
  ratio_ = get_real_detail("ratio", .333);  
  between_length_limit_ = get_real_detail ("between-length-limit", 1.0);
  
  wrong_direction_offset_penalty_ = get_real_detail("wrong-direction-offset-penalty", 10);
  distance_penalty_factor_ = get_real_detail("distance-penalty-factor", 5);
  length_penalty_factor_ = get_real_detail("length-penalty-factor", 1.0);
  min_length_ = get_real_detail("min-length", 0.333);

  // in half-space
  staff_line_clearance_ = get_real_detail ("staff-line-clearance", 0.4);
  staff_line_collision_penalty_ = get_real_detail("staff-line-collision-penalty", 5);
  dot_collision_clearance_ = get_real_detail ( "dot-collision-clearance", 0.25);
  dot_collision_penalty_ = get_real_detail ( "dot-collision-penalty", 0.25);

  tie_column_monotonicity_penalty_ = get_real_detail ("tie-column-monotonicity-penalty", 100);
  tie_tie_collision_penalty_ = get_real_detail ("tie-tie-collision-penalty", 30);
  tie_tie_collision_distance_ = get_real_detail ("tie-tie-collision-distance", .25);
}

Tie_details::Tie_details ()
{
  staff_space_ = 1.0; 
  height_limit_ = 1.0;
  ratio_ = .333;   
}

