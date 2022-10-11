/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef BEAM_SCORING_PROBLEM_HH
#define BEAM_SCORING_PROBLEM_HH

#include "beam.hh"
#include "interval.hh"
#include "lily-guile.hh"
#include "lily-proto.hh"
#include "stem-info.hh"

#include <memory>
#include <vector>

enum Scorers
{
  // Should be ordered by increasing expensiveness.
  ORIGINAL_DISTANCE,
  SLOPE_IDEAL,
  SLOPE_MUSICAL,
  SLOPE_DIRECTION,
  HORIZONTAL_INTER,
  FORBIDDEN,
  STEM_LENGTHS,
  COLLISIONS,
  NUM_SCORERS,
};

class Beam_configuration
{
public:
  Drul_array<Real> y;
  Real demerits;
  std::string score_card_;

  int next_scorer_todo;

  Beam_configuration ();
  bool done () const;
  void add (Real demerit, const std::string &reason);
  static std::unique_ptr<Beam_configuration>
  new_config (Drul_array<Real> start, Drul_array<Real> offset);
};

// Comparator for a queue of Beam_configuration*.
class Beam_configuration_less
{
public:
  bool operator() (Beam_configuration *const &l, Beam_configuration *const &r)
  {
    // Invert
    return l->demerits > r->demerits;
  }
};

class Beam_quant_parameters
{
public:
  Real SECONDARY_BEAM_DEMERIT;
  Real STEM_LENGTH_DEMERIT_FACTOR;
  Real REGION_SIZE;

  /*
    threshold to combat rounding errors.
  */
  Real BEAM_EPS;

  // possibly ridiculous, but too short stems just won't do
  Real STEM_LENGTH_LIMIT_PENALTY;
  Real DAMPING_DIRECTION_PENALTY;
  Real MUSICAL_DIRECTION_FACTOR;
  Real HINT_DIRECTION_PENALTY;
  Real IDEAL_SLOPE_FACTOR;
  Real ROUND_TO_ZERO_SLOPE;
  Real COLLISION_PENALTY;
  Real COLLISION_PADDING;
  Real HORIZONTAL_INTER_QUANT_PENALTY;
  Real STEM_COLLISION_FACTOR;

  void fill (Grob *him);
};

struct Beam_collision
{
  Real x_;
  Interval y_;
  Real base_penalty_;

  // Need to add beam_config->y to get actual offsets.
  Interval beam_y_;
};

/*
  Parameters for a single beam.  Precomputed to save time in
  scoring individual configurations.

  */
class Beam_scoring_problem
{
public:
  Beam_scoring_problem (Grob *me, Drul_array<Real> ys, bool);
  Drul_array<Real> solve () const;

private:
  Spanner *beam_;

  Drul_array<Real> unquanted_y_;
  bool align_broken_intos_;
  bool do_initial_slope_calculations_;

  Real staff_space_;
  Real beam_thickness_;
  Real line_thickness_;
  Real musical_dy_;
  vsize normal_stem_count_;
  Real x_span_;

  /*
    Do stem computations.  These depend on YL and YR linearly, so we can
    precompute for every stem 2 factors.

    We store some info to quickly interpolate.  The stemlengths are
    affine linear in YL and YR. If YL == YR == 0, then we might have
    stem_y != 0.0, when we're cross staff.
  */
  std::vector<Stem_info> stem_infos_;
  std::vector<Real> chord_start_y_;
  std::vector<Interval> head_positions_;
  std::vector<Slice> beam_multiplicity_;
  std::vector<bool> is_normal_;
  std::vector<Real> base_lengths_;
  std::vector<Real> stem_xpositions_;
  std::vector<Real> stem_ypositions_;

  bool is_xstaff_;
  bool is_knee_;

  Beam_quant_parameters parameters_;

  Real staff_radius_;
  Drul_array<int> edge_beam_counts_;
  Drul_array<Direction> edge_dirs_;

  // Half-open intervals, representing allowed positions for the beam,
  // starting from close to the notehead to the direction of the stem
  // end.  This is used for quickly weeding out invalid
  // Beam_configurations.
  Drul_array<Interval> quant_range_;
  int max_beam_count_;
  Real length_fraction_;
  Real beam_translation_;
  std::vector<Beam_collision> collisions_;
  std::vector<Beam_segment> segments_;

  vsize first_normal_index ();
  vsize last_normal_index ();

  void init_instance_variables (Grob *me, Drul_array<Real> ys,
                                bool align_broken_intos);
  void add_collision (Real x, Interval y, Real factor);
  void no_visible_stem_positions ();
  void least_squares_positions ();
  Real calc_concaveness ();
  void slope_damping ();
  void shift_region_to_valid ();

  void one_scorer (Beam_configuration *config) const;
  Beam_configuration *force_score (
    SCM inspect_quants,
    const std::vector<std::unique_ptr<Beam_configuration>> &configs) const;
  Real y_at (Real x, Beam_configuration const *c) const;

  // Scoring functions:
  void score_forbidden_quants (Beam_configuration *config) const;
  void score_horizontal_inter_quants (Beam_configuration *config) const;
  void score_slope_ideal (Beam_configuration *config) const;
  void score_slope_direction (Beam_configuration *config) const;
  void score_slope_musical (Beam_configuration *config) const;
  void score_stem_lengths (Beam_configuration *config) const;
  void generate_quants (
    std::vector<std::unique_ptr<Beam_configuration>> *scores) const;
  void score_collisions (Beam_configuration *config) const;
};

#endif /* BEAM_SCORING_PROBLEM_HH */
