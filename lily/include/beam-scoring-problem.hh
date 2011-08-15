/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2011 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
#include "main.hh"  //  DEBUG_BEAM_SCORING
#include "std-vector.hh"
#include "stem-info.hh"

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

struct Beam_configuration
{
  Interval y;
  Real demerits;
#if DEBUG_BEAM_SCORING
  string score_card_;
#endif

  int next_scorer_todo;

  Beam_configuration ();
  bool done () const;
  void add (Real demerit, const string &reason);
  static Beam_configuration *new_config (Interval start,
                                         Interval offset);
};

// Comparator for a queue of Beam_configuration*.
class Beam_configuration_less
{
public:
  bool operator () (Beam_configuration *const &l, Beam_configuration *const &r)
  {
    // Invert
    return l->demerits > r->demerits;
  }
};

struct Beam_quant_parameters
{
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

  TODO - use trailing _ on data members.

  */
class Beam_scoring_problem
{
public:
  Beam_scoring_problem (Grob *me, Drul_array<Real> ys);
  Drul_array<Real> solve () const;

private:
  Grob *beam;

  Interval unquanted_y;

  Real staff_space;
  Real beam_thickness;
  Real line_thickness;
  Real musical_dy;

  Interval x_span;

  vector<Stem_info> stem_infos;

  /*
    Do stem computations.  These depend on YL and YR linearly, so we can
    precompute for every stem 2 factors.

    We store some info to quickly interpolate.  The stemlengths are
    affine linear in YL and YR. If YL == YR == 0, then we might have
    stem_y != 0.0, when we're cross staff.
  */
  vector<Real> base_lengths;
  vector<Real> stem_xpositions;

  Grob *common[2];
  bool is_xstaff;
  bool is_knee;

  Beam_quant_parameters parameters;

  Real staff_radius;
  Drul_array<int> edge_beam_counts;
  Drul_array<Direction> edge_dirs;

  // Half-open intervals, representing allowed positions for the beam,
  // starting from close to the notehead to the direction of the stem
  // end.  This is used for quickly weeding out invalid
  // Beam_configurations.
  Drul_array<Interval> quant_range;
  Real beam_translation;
  vector<Beam_collision> collisions_;
  vector<Beam_segment> segments_;

  void init_stems ();
  void init_collisions (vector<Grob *> grobs);
  void add_collision (Real x, Interval y, Real factor);

  void one_scorer (Beam_configuration *config) const;
  Beam_configuration *force_score (SCM inspect_quants,
                                   const vector<Beam_configuration *> &configs) const;
  Real y_at (Real x, Beam_configuration const *c) const;

  // Scoring functions:
  void score_forbidden_quants (Beam_configuration *config) const;
  void score_horizontal_inter_quants (Beam_configuration *config) const;
  void score_slope_ideal (Beam_configuration *config) const;
  void score_slope_direction (Beam_configuration *config) const;
  void score_slope_musical (Beam_configuration *config) const;
  void score_stem_lengths (Beam_configuration *config) const;
  void generate_quants (vector<Beam_configuration *>* scores) const;
  void score_collisions (Beam_configuration *config) const;
};

#endif /* BEAM_SCORING_PROBLEM_HH */
