/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2011 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#include "beam-scoring-problem.hh"

#include <algorithm>
#include <queue>
#include <set>
using namespace std;

#include "align-interface.hh"
#include "beam.hh"
#include "direction.hh"
#include "directional-element-interface.hh"
#include "grob.hh"
#include "grob-array.hh"
#include "item.hh"
#include "international.hh"
#include "least-squares.hh"
#include "libc-extension.hh"
#include "main.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stencil.hh"
#include "stem.hh"
#include "warn.hh"

Real
get_detail (SCM alist, SCM sym, Real def)
{
  SCM entry = scm_assq (sym, alist);

  if (scm_is_pair (entry))
    return robust_scm2double (scm_cdr (entry), def);
  return def;
}

void
Beam_quant_parameters::fill (Grob *him)
{
  SCM details = him->get_property ("details");

  // General
  BEAM_EPS = get_detail (details, ly_symbol2scm ("beam-eps"), 1e-3);
  REGION_SIZE = get_detail (details, ly_symbol2scm ("region-size"), 2);

  // forbidden quants
  SECONDARY_BEAM_DEMERIT = get_detail (details, ly_symbol2scm ("secondary-beam-demerit"), 10.0);
  STEM_LENGTH_DEMERIT_FACTOR = get_detail (details, ly_symbol2scm ("stem-length-demerit-factor"), 5);
  HORIZONTAL_INTER_QUANT_PENALTY = get_detail (details, ly_symbol2scm ("horizontal-inter-quant"), 500);

  STEM_LENGTH_LIMIT_PENALTY = get_detail (details, ly_symbol2scm ("stem-length-limit-penalty"), 5000);
  DAMPING_DIRECTION_PENALTY = get_detail (details, ly_symbol2scm ("damping-direction-penalty"), 800);
  HINT_DIRECTION_PENALTY = get_detail (details, ly_symbol2scm ("hint-direction-penalty"), 20);
  MUSICAL_DIRECTION_FACTOR = get_detail (details, ly_symbol2scm ("musical-direction-factor"), 400);
  IDEAL_SLOPE_FACTOR = get_detail (details, ly_symbol2scm ("ideal-slope-factor"), 10);
  ROUND_TO_ZERO_SLOPE = get_detail (details, ly_symbol2scm ("round-to-zero-slope"), 0.02);

  // Collisions
  COLLISION_PENALTY = get_detail (details, ly_symbol2scm ("collision-penalty"), 500);
  COLLISION_PADDING = get_detail (details, ly_symbol2scm ("collision-padding"), 0.5);
  STEM_COLLISION_FACTOR = get_detail (details, ly_symbol2scm ("stem-collision-factor"), 0.1);
}

// Add x if x is positive, add |x|*fac if x is negative.
static Real
shrink_extra_weight (Real x, Real fac)
{
  return fabs (x) * ((x < 0) ? fac : 1.0);
}

/****************************************************************/

Beam_configuration::Beam_configuration ()
{
  y = Interval (0.0, 0.0);
  demerits = 0.0;
  next_scorer_todo = ORIGINAL_DISTANCE;
}

bool Beam_configuration::done () const
{
  return next_scorer_todo >= NUM_SCORERS;
}

void Beam_configuration::add (Real demerit, const string &reason)
{
  demerits += demerit;

#if DEBUG_BEAM_SCORING
  if (demerit)
    score_card_ += to_string (" %s %.2f", reason.c_str (), demerit);
#endif
}

Beam_configuration *Beam_configuration::new_config (Interval start,
                                                    Interval offset)
{
  Beam_configuration *qs = new Beam_configuration;
  qs->y = Interval (int (start[LEFT]) + offset[LEFT],
                    int (start[RIGHT]) + offset[RIGHT]);

  // This orders the sequence so we try combinations closest to the
  // the ideal offset first.
  Real start_score = abs (offset[RIGHT]) + abs (offset[LEFT]);
  qs->demerits = start_score / 1000.0;
  qs->next_scorer_todo = ORIGINAL_DISTANCE + 1;

  return qs;
}

Real
Beam_scoring_problem::y_at (Real x, Beam_configuration const *p) const
{
  return p->y[LEFT] + x * p->y.delta () / x_span_;
}

/****************************************************************/

/*
  TODO:

  - Make all demerits customisable

  - Add demerits for quants per se, as to forbid a specific quant
  entirely
*/

// This is a temporary hack to see how much we can gain by using a
// priority queue on the beams to score.
static int score_count = 0;
LY_DEFINE (ly_beam_score_count, "ly:beam-score-count", 0, 0, 0,
           (),
           "count number of beam scores.")
{
  return scm_from_int (score_count);
}

void Beam_scoring_problem::add_collision (Real x, Interval y,
                                          Real score_factor)
{
  // We used to screen for quant range, but no more.

  Beam_collision c;
  c.beam_y_.set_empty ();

  for (vsize j = 0; j < segments_.size (); j++)
    {
      if (segments_[j].horizontal_.contains (x))
        c.beam_y_.add_point (segments_[j].vertical_count_ * beam_translation_);
      if (segments_[j].horizontal_[LEFT] > x)
        break;
    }
  c.beam_y_.widen (0.5 * beam_thickness_);

  c.x_ = x;

  y *= 1 / staff_space_;
  c.y_ = y;
  c.base_penalty_ = score_factor;
  collisions_.push_back (c);
}

void Beam_scoring_problem::init_stems ()
{
  vector<Spanner *> beams;
  if (consistent_broken_slope_)
    {
      Spanner *orig = dynamic_cast<Spanner *> (beam_->original ());
      if (!orig)
        consistent_broken_slope_ = false;
      else if (!orig->broken_intos_.size ())
        consistent_broken_slope_ = false;
      else
        beams.insert (beams.end (), orig->broken_intos_.begin (), orig->broken_intos_.end ());
    }
  if (!consistent_broken_slope_)
    beams.push_back (beam_);

  x_span_ = 0.0;
  normal_stem_count_ = 0;
  for (vsize i = 0; i < beams.size (); i++)
    {
      Interval local_x_span;
      extract_grob_set (beams[i], "stems", stems);
      extract_grob_set (beams[i], "covered-grobs", fake_collisions);
      vector<Grob *> collisions;

      for (vsize j = 0; j < fake_collisions.size (); j++)
        if (fake_collisions[j]->get_system () == beams[i]->get_system ())
          collisions.push_back (fake_collisions[j]);

      Grob *common[2];
      for (int a = 2; a--;)
        common[a] = common_refpoint_of_array (stems, beams[i], Axis (a));

      Real x_left = beams[i]->relative_coordinate(common[X_AXIS], X_AXIS);

      Drul_array<Grob *> edge_stems (Beam::first_normal_stem (beams[i]),
                                     Beam::last_normal_stem (beams[i]));
      Direction d = LEFT;
      do
        local_x_span[d] = edge_stems[d] ? edge_stems[d]->relative_coordinate (common[X_AXIS], X_AXIS) : 0.0;
      while (flip (&d) != LEFT);

      Drul_array<bool> dirs_found (0, 0);

      Real my_y = beams[i]->relative_coordinate (common[Y_AXIS], Y_AXIS);

      Interval beam_width (-1.0,-1.0);
      for (vsize j = 0; j < stems.size (); j++)
        {
          Grob *s = stems[j];
          beam_multiplicity_.push_back (Stem::beam_multiplicity (stems[j]));
          head_positions_.push_back (Stem::head_positions (stems[j]));
          is_normal_.push_back (Stem::is_normal_stem (stems[j]));

          Stem_info si (Stem::get_stem_info (s));
          si.scale (1 / staff_space_);
          stem_infos_.push_back (si);
          chord_start_y_.push_back (Stem::chord_start_y (s));
          dirs_found[si.dir_] = true;

          bool f = to_boolean (s->get_property ("french-beaming"))
                   && s != edge_stems[LEFT] && s != edge_stems[RIGHT];

          Real y = Beam::calc_stem_y (beams[i], s, common, local_x_span[LEFT], local_x_span[RIGHT], CENTER,
                                      Interval (0, 0), f);
          base_lengths_.push_back (y / staff_space_);
          stem_xpositions_.push_back (s->relative_coordinate (common[X_AXIS], X_AXIS) - x_left + x_span_);
          stem_ypositions_.push_back (s->relative_coordinate (common[Y_AXIS], Y_AXIS) - my_y);
          if (is_normal_.back ())
            {
              if (beam_width[LEFT] == -1.0)
                beam_width[LEFT] = stem_xpositions_.back ();
              beam_width[RIGHT] = stem_xpositions_.back ();
            }
        }

      edge_dirs_ = Drul_array<Direction> (CENTER, CENTER);
      normal_stem_count_ += Beam::normal_stem_count (beams[i]);
      if (normal_stem_count_)
        edge_dirs_ = Drul_array<Direction> (stem_infos_[0].dir_,
                                            stem_infos_.back ().dir_);

      is_xstaff_ = Align_interface::has_interface (common[Y_AXIS]);
      is_knee_ = dirs_found[LEFT] && dirs_found[RIGHT];

      staff_radius_ = Staff_symbol_referencer::staff_radius (beams[i]);
      edge_beam_counts_ = Drul_array<int>
                         (Stem::beam_multiplicity (stems[0]).length () + 1,
                          Stem::beam_multiplicity (stems.back ()).length () + 1);

      // TODO - why are we dividing by staff_space_?
      beam_translation_ = Beam::get_beam_translation (beams[i]) / staff_space_;

      d = LEFT;
      do
        {
          quant_range_[d].set_full ();
          if (!edge_stems[d])
            continue;

          Real stem_offset = edge_stems[d]->relative_coordinate (common[Y_AXIS], Y_AXIS)
                             - beams[i]->relative_coordinate (common[Y_AXIS], Y_AXIS);
          Interval heads = Stem::head_positions (edge_stems[d]) * 0.5 * staff_space_;

          Direction ed = edge_dirs_[d];
          heads.widen (0.5 * staff_space_
                       + (edge_beam_counts_[d] - 1) * beam_translation_ + beam_thickness_ * .5);
          quant_range_[d][-ed] = heads[ed] + stem_offset;
        }
      while (flip (&d) != LEFT);
      Grob *common_x = NULL;
      segments_ = Beam::get_beam_segments (beams[i], &common_x);
      vector_sort (segments_, beam_segment_less);
      for (vsize j = 0; j < segments_.size (); j++)
        segments_[j].horizontal_ += (x_span_ - x_left);

      set<Grob *> colliding_stems;
      for (vsize j = 0; j < collisions.size (); j++)
        {
          if (!collisions[j]->is_live ())
            continue;

          if (Beam::has_interface (collisions[j]) && Beam::is_cross_staff (collisions[j]))
            continue;

          Box b;
          for (Axis a = X_AXIS; a < NO_AXES; incr (a))
            b[a] = collisions[j]->extent (common[a], a);

          if (b[X_AXIS].is_empty () || b[Y_AXIS].is_empty ())
            continue;

          b[X_AXIS] += (x_span_ - x_left);
          Real width = b[X_AXIS].length ();
          Real width_factor = sqrt (width / staff_space_);

          Direction d = LEFT;
          do
            add_collision (b[X_AXIS][d], b[Y_AXIS], width_factor);
          while (flip (&d) != LEFT);

          Grob *stem = unsmob_grob (collisions[j]->get_object ("stem"));
          if (stem && Stem::has_interface (stem) && Stem::is_normal_stem (stem))
            {
              colliding_stems.insert (stem);
            }
        }

      for (set<Grob *>::const_iterator it (colliding_stems.begin ()); it != colliding_stems.end (); it++)
        {
          Grob *s = *it;
          Real x = (s->extent (common[X_AXIS], X_AXIS) - x_left + x_span_).center ();

          Direction stem_dir = get_grob_direction (*it);
          Interval y;
          y.set_full ();
          y[-stem_dir] = Stem::chord_start_y (*it) + (*it)->relative_coordinate (common[Y_AXIS], Y_AXIS)
                         - beams[i]->relative_coordinate (common[Y_AXIS], Y_AXIS);

          Real factor = parameters_.STEM_COLLISION_FACTOR;
          if (!unsmob_grob (s->get_object ("beam")))
            factor = 1.0;
          add_collision (x, y, factor);
        }
      x_span_ += beams[i]->spanner_length ();
    }

  /*
    Here, we eliminate all extremal hangover, be it from non-normal stems
    (like stemlets) or broken beams (if we're not calculating consistent
    slope).
  */
  if (normal_stem_count_)
    {
      Interval trimmings (0.0, 0.0);
      Direction d = LEFT;

      do
        {
          vsize idx = d == LEFT ? first_normal_index () : last_normal_index ();
          trimmings[d] = d * ((d == LEFT ? 0 : x_span_) - stem_xpositions_[idx]);
        }
      while (flip (&d) != LEFT);

      do
        x_span_ -= trimmings[d];
      while (flip (&d) != LEFT);

      for (vsize i = 0; i < stem_xpositions_.size (); i++)
        stem_xpositions_[i] -= trimmings[LEFT];
    }
}

Beam_scoring_problem::Beam_scoring_problem (Grob *me, Drul_array<Real> ys)
{
  beam_ = dynamic_cast<Spanner *> (me);
  unquanted_y_ = ys;
  consistent_broken_slope_ = to_boolean (me->get_property ("consistent-broken-slope"));
  /*
    Calculations are relative to a unit-scaled staff, i.e. the quants are
    divided by the current staff_space_.
  */
  staff_space_ = Staff_symbol_referencer::staff_space (me);
  beam_thickness_ = Beam::get_beam_thickness (me) / staff_space_;
  line_thickness_ = Staff_symbol_referencer::line_thickness (me) / staff_space_;

  // This is the least-squares DY, corrected for concave beams.
  musical_dy_ = robust_scm2double (me->get_property ("least-squares-dy"), 0);

  parameters_.fill (me);
  init_stems ();
  least_squares_positions ();
  slope_damping ();
  shift_region_to_valid ();
}

// Assuming V is not empty, pick a 'reasonable' point inside V.
static Real
point_in_interval (Interval v, Real dist)
{
  if (isinf (v[DOWN]))
    return v[UP] - dist;
  else if (isinf (v[UP]))
    return v[DOWN] + dist;
  else
    return v.center ();
}

/* Set stem's shorten property if unset.

TODO:
take some y-position (chord/beam/nearest?) into account
scmify forced-fraction

This is done in beam because the shorten has to be uniform over the
entire beam.
*/

void
set_minimum_dy (Grob *me, Real *dy)
{
  if (*dy)
    {
      /*
        If dy is smaller than the smallest quant, we
        get absurd direction-sign penalties.
      */

      Real ss = Staff_symbol_referencer::staff_space (me);
      Real beam_thickness = Beam::get_beam_thickness (me) / ss;
      Real slt = Staff_symbol_referencer::line_thickness (me) / ss;
      Real sit = (beam_thickness - slt) / 2;
      Real inter = 0.5;
      Real hang = 1.0 - (beam_thickness - slt) / 2;

      *dy = sign (*dy) * max (fabs (*dy),
                              min (min (sit, inter), hang));
    }
}

void
Beam_scoring_problem::no_visible_stem_positions ()
{
  if (!head_positions_.size ())
    {
      unquanted_y_ = Interval (0, 0);
      return;
    }

  Interval head_positions;
  Slice multiplicity;
  for (vsize i = 0; i < head_positions_.size (); i++)
    {
      head_positions.unite (head_positions_[i]);
      multiplicity.unite (beam_multiplicity_[i]);
    }

  Direction dir = get_grob_direction (beam_);

  if (!dir)
    programming_error ("The beam should have a direction by now.");

  Real y = head_positions.linear_combination (dir)
           * 0.5 * staff_space_
           + dir * beam_translation_ * (multiplicity.length () + 1);

  unquanted_y_ = Interval (y, y);
}

vsize
Beam_scoring_problem::first_normal_index ()
{
  for (vsize i = 0; i < is_normal_.size (); i++)
    if (is_normal_[i])
      return i;

  beam_->programming_error ("No normal stems, but asking for first normal stem index.");
  return 0;
}

vsize
Beam_scoring_problem::last_normal_index ()
{
  for (vsize i = is_normal_.size (); i--;)
    if (is_normal_[i])
      return i;

  beam_->programming_error ("No normal stems, but asking for first normal stem index.");
  return 0;
}

void
Beam_scoring_problem::least_squares_positions ()
{
  if (!normal_stem_count_)
    {
      no_visible_stem_positions ();
      return;
    }

  if (stem_infos_.size () < 1)
    return;

  vsize fnx = first_normal_index ();
  vsize lnx = last_normal_index ();

  Interval ideal (stem_infos_[fnx].ideal_y_ + stem_ypositions_[fnx],
                  stem_infos_[lnx].ideal_y_ + stem_ypositions_[lnx]);

  Real y = 0;
  Real slope = 0;
  Real dy = 0;
  Real ldy = 0.0;
  if (!ideal.delta ())
    {
      Interval chord (chord_start_y_[0],
                      chord_start_y_.back ());

      /* Simple beams (2 stems) on middle line should be allowed to be
         slightly sloped.

         However, if both stems reach middle line,
         ideal[LEFT] == ideal[RIGHT] and ideal.delta () == 0.

         For that case, we apply artificial slope */
      if (!ideal[LEFT] && chord.delta () && stem_infos_.size () == 2)
        {
          /* FIXME. -> UP */
          Direction d = (Direction) (sign (chord.delta ()) * UP);
          unquanted_y_[d] = Beam::get_beam_thickness (beam_) / 2;
          unquanted_y_[-d] = -unquanted_y_[d];
        }
      else
        unquanted_y_ = ideal;

      /*
        For broken beams this doesn't work well. In this case, the
        slope esp. of the first part of a broken beam should predict
        where the second part goes.
      */
      ldy = unquanted_y_[RIGHT] - unquanted_y_[LEFT];
    }
  else
    {
      vector<Offset> ideals;
      for (vsize i = 0; i < stem_infos_.size (); i++)
        if (is_normal_[i])
          ideals.push_back (Offset (stem_xpositions_[i],
                                    stem_infos_[i].ideal_y_
                                    + stem_ypositions_[i]));

      minimise_least_squares (&slope, &y, ideals);

      dy = slope * x_span_;

      set_minimum_dy (beam_, &dy);

      ldy = dy;
      unquanted_y_ = Interval (y, (y + dy));
    }

  musical_dy_ = ldy;
}

void
Beam_scoring_problem::slope_damping ()
{
  if (normal_stem_count_ <= 1)
    return;

  SCM s = beam_->get_property ("damping");
  Real damping = scm_to_double (s);
  Real concaveness = robust_scm2double (beam_->get_property ("concaveness"), 0.0);
  if (concaveness >= 10000)
    {
      unquanted_y_[LEFT] = unquanted_y_[RIGHT];
      musical_dy_ = 0;
      damping = 0;
    }

  if (damping)
    {
      Real dy = unquanted_y_[RIGHT] - unquanted_y_[LEFT];

      Real slope = dy && x_span_ ? dy / x_span_ : 0;

      slope = 0.6 * tanh (slope) / (damping + concaveness);

      Real damped_dy = slope * x_span_;

      set_minimum_dy (beam_, &damped_dy);

      unquanted_y_[LEFT] += (dy - damped_dy) / 2;
      unquanted_y_[RIGHT] -= (dy - damped_dy) / 2;
    }
}

void
Beam_scoring_problem::shift_region_to_valid ()
{
  if (!normal_stem_count_)
    return;

  Real beam_dy = unquanted_y_[RIGHT] - unquanted_y_[LEFT];
  Real slope = x_span_ ? beam_dy / x_span_ : 0.0;

  /*
    Shift the positions so that we have a chance of finding good
    quants (i.e. no short stem failures.)
  */
  Interval feasible_left_point;
  feasible_left_point.set_full ();

  for (vsize i = 0; i < stem_infos_.size (); i++)
    {
      // TODO - check for invisible here...
      Real left_y
        = stem_infos_[i].shortest_y_
          - slope * stem_xpositions_ [i];

      /*
        left_y is now relative to the stem S. We want relative to
        ourselves, so translate:
      */
      left_y += stem_ypositions_[i];
      Interval flp;
      flp.set_full ();
      flp[-stem_infos_[i].dir_] = left_y;

      feasible_left_point.intersect (flp);
    }

  vector<Grob *> filtered;
  /*
    We only update these for objects that are too large for quanting
    to find a workaround.  Typically, these are notes with
    stems, and timesig/keysig/clef, which take out the entire area
    inside the staff as feasible.

    The code below disregards the thickness and multiplicity of the
    beam.  This should not be a problem, as the beam quanting will
    take care of computing the impact those exactly.
  */
  Real min_y_size = 2.0;

  // A list of intervals into which beams may not fall
  vector<Interval> forbidden_intervals;

  for (vsize i = 0; i < collisions_.size (); i++)
    {
      if (collisions_[i].x_ < 0 || collisions_[i].x_ > x_span_)
        continue;

      if (collisions_[i].y_.length () < min_y_size)
        continue;

      Direction d = LEFT;
      do
        {
          Real dy = slope * collisions_[i].x_;

          Direction yd = DOWN;
          Interval disallowed;
          do
            {
              Real left_y = collisions_[i].y_[yd] - dy;
              disallowed[yd] = left_y;
            }
          while (flip (&yd) != DOWN);

          forbidden_intervals.push_back (disallowed);
        }
      while (flip (&d) != LEFT);
    }

  vector_sort (forbidden_intervals, Interval::left_less);
  Real epsilon = 1.0e-10;
  Real beam_left_y = unquanted_y_[LEFT];
  Interval feasible_beam_placements (beam_left_y, beam_left_y);

  /*
    forbidden_intervals contains a vector of intervals in which
    the beam cannot start.  it iterates through these intervals,
    pushing feasible_beam_placements epsilon over or epsilon under a
    collision.  when this type of change happens, the loop is marked
    as "dirty" and re-iterated.

    TODO: figure out a faster ways that this loop can happen via
    a better search algorithm and/or OOP.
  */

  bool dirty = false;
  do
    {
      dirty = false;
      for (vsize i = 0; i < forbidden_intervals.size (); i++)
        {
          Direction d = DOWN;
          do
            {
              if (forbidden_intervals[i][d] == d * infinity_f)
                feasible_beam_placements[d] = d * infinity_f;
              else if (forbidden_intervals[i].contains (feasible_beam_placements[d]))
                {
                  feasible_beam_placements[d] = d * epsilon + forbidden_intervals[i][d];
                  dirty = true;
                }
            }
          while (flip (&d) != DOWN);
        }
    }
  while (dirty);

  // if the beam placement falls out of the feasible region, we push it
  // to infinity so that it can never be a feasible candidate below
  Direction d = DOWN;
  do
    {
      if (!feasible_left_point.contains (feasible_beam_placements[d]))
        feasible_beam_placements[d] = d * infinity_f;
    }
  while (flip (&d) != DOWN);

  if ((feasible_beam_placements[UP] == infinity_f && feasible_beam_placements[DOWN] == -infinity_f) && !feasible_left_point.is_empty ())
    {
      // We are somewhat screwed: we have a collision, but at least
      // there is a way to satisfy stem length constraints.
      beam_left_y = point_in_interval (feasible_left_point, 2.0);
    }
  else if (!feasible_left_point.is_empty ())
    {
      // Only one of them offers is feasible solution. Pick that one.
      if (abs (beam_left_y - feasible_beam_placements[DOWN]) > abs (beam_left_y - feasible_beam_placements[UP]))
        beam_left_y = feasible_beam_placements[UP];
      else
        beam_left_y = feasible_beam_placements[DOWN];
    }
  else
    {
      // We are completely screwed.
      beam_->warning (_ ("no viable initial configuration found: may not find good beam slope"));
    }

  unquanted_y_ = Drul_array<Real> (beam_left_y, (beam_left_y + beam_dy));
}

void
Beam_scoring_problem::generate_quants (vector<Beam_configuration *> *scores) const
{
  int region_size = (int) parameters_.REGION_SIZE;

  // Knees and collisions are harder, lets try some more possibilities
  if (is_knee_)
    region_size += 2;
  if (collisions_.size ())
    region_size += 2;

  Real straddle = 0.0;
  Real sit = (beam_thickness_ - line_thickness_) / 2;
  Real inter = 0.5;
  Real hang = 1.0 - (beam_thickness_ - line_thickness_) / 2;
  Real base_quants [] = {straddle, sit, inter, hang};
  int num_base_quants = int (sizeof (base_quants) / sizeof (Real));

  /*
    Asymetry ? should run to <= region_size ?
  */
  vector<Real> unshifted_quants;
  for (int i = -region_size; i < region_size; i++)
    for (int j = 0; j < num_base_quants; j++)
      {
        unshifted_quants.push_back (i + base_quants[j]);
      }

  for (vsize i = 0; i < unshifted_quants.size (); i++)
    for (vsize j = 0; j < unshifted_quants.size (); j++)
      {
        Beam_configuration *c
          = Beam_configuration::new_config (unquanted_y_,
                                            Interval (unshifted_quants[i],
                                                      unshifted_quants[j]));

        Direction d = LEFT;
        do
          {
            if (!quant_range_[d].contains (c->y[d]))
              {
                delete c;
                c = NULL;
                break;
              }
          }
        while (flip (&d) != LEFT);
        if (c)
          scores->push_back (c);
      }

}

void Beam_scoring_problem::one_scorer (Beam_configuration *config) const
{
  score_count++;
  switch (config->next_scorer_todo)
    {
    case SLOPE_IDEAL:
      score_slope_ideal (config);
      break;
    case SLOPE_DIRECTION:
      score_slope_direction (config);
      break;
    case SLOPE_MUSICAL:
      score_slope_musical (config);
      break;
    case FORBIDDEN:
      score_forbidden_quants (config);
      break;
    case STEM_LENGTHS:
      score_stem_lengths (config);
      break;
    case COLLISIONS:
      score_collisions (config);
      break;
    case HORIZONTAL_INTER:
      score_horizontal_inter_quants (config);
      break;

    case NUM_SCORERS:
    case ORIGINAL_DISTANCE:
    default:
      assert (false);
    }
  config->next_scorer_todo++;
}

Beam_configuration *
Beam_scoring_problem::force_score (SCM inspect_quants, const vector<Beam_configuration *> &configs) const
{
  Drul_array<Real> ins = ly_scm2interval (inspect_quants);
  Real mindist = 1e6;
  Beam_configuration *best = NULL;
  for (vsize i = 0; i < configs.size (); i++)
    {
      Real d = fabs (configs[i]->y[LEFT] - ins[LEFT]) + fabs (configs[i]->y[RIGHT] - ins[RIGHT]);
      if (d < mindist)
        {
          best = configs[i];
          mindist = d;
        }
    }
  if (mindist > 1e5)
    programming_error ("cannot find quant");

  while (!best->done ())
    one_scorer (best);

  return best;
}

Drul_array<Real>
Beam_scoring_problem::solve () const
{
  vector<Beam_configuration *> configs;
  generate_quants (&configs);

  if (configs.empty ())
    {
      programming_error ("No viable beam quanting found.  Using unquanted y value.");
      return unquanted_y_;
    }

  if (to_boolean (beam_->get_property ("skip-quanting")))
    return unquanted_y_;

  Beam_configuration *best = NULL;

  bool debug
    = to_boolean (beam_->layout ()->lookup_variable (ly_symbol2scm ("debug-beam-scoring")));
  SCM inspect_quants = beam_->get_property ("inspect-quants");
  if (scm_is_pair (inspect_quants))
    {
      debug = true;
      best = force_score (inspect_quants, configs);
    }
  else
    {
      std::priority_queue < Beam_configuration *, std::vector<Beam_configuration *>,
          Beam_configuration_less > queue;
      for (vsize i = 0; i < configs.size (); i++)
        queue.push (configs[i]);

      /*
        TODO

        It would be neat if we generated new configurations on the
        fly, depending on the best complete score so far, eg.

        if (best->done()) {
          if (best->demerits < sqrt(queue.size())
            break;
          while (best->demerits > sqrt(queue.size()) {
            generate and insert new configuration
          }
        }

        that would allow us to do away with region_size altogether.
      */
      while (true)
        {
          best = queue.top ();
          if (best->done ())
            break;

          queue.pop ();
          one_scorer (best);
          queue.push (best);
        }
    }

  Interval final_positions = best->y;

#if DEBUG_BEAM_SCORING
  if (debug)
    {
      // debug quanting
      int completed = 0;
      for (vsize i = 0; i < configs.size (); i++)
        {
          if (configs[i]->done ())
            completed++;
        }

      string card = best->score_card_ + to_string (" c%d/%d", completed, configs.size ());
      beam_->set_property ("annotation", ly_string2scm (card));
    }
#endif

  junk_pointers (configs);
  if (consistent_broken_slope_)
    {
      Interval normalized_endpoints = robust_scm2interval (beam_->get_property ("normalized-endpoints"), Interval (0, 1));
      Real y_length = final_positions[RIGHT] - final_positions[LEFT];

      final_positions[LEFT] += normalized_endpoints[LEFT] * y_length;
      final_positions[RIGHT] -= (1 - normalized_endpoints[RIGHT]) * y_length;
    }

  return final_positions;
}

void
Beam_scoring_problem::score_stem_lengths (Beam_configuration *config) const
{
  Real limit_penalty = parameters_.STEM_LENGTH_LIMIT_PENALTY;
  Drul_array<Real> score (0, 0);
  Drul_array<int> count (0, 0);

  for (vsize i = 0; i < stem_xpositions_.size (); i++)
    {
      if (!is_normal_[i])
        continue;

      Real x = stem_xpositions_[i];
      Real dx = x_span_;
      Real beam_y = dx
                    ? config->y[RIGHT] * x / dx + config->y[LEFT] * (x_span_ - x) / dx
                    : (config->y[RIGHT] + config->y[LEFT]) / 2;
      Real current_y = beam_y + base_lengths_[i];
      Real length_pen = parameters_.STEM_LENGTH_DEMERIT_FACTOR;

      Stem_info info = stem_infos_[i];
      Direction d = info.dir_;

      score[d] += limit_penalty * max (0.0, (d * (info.shortest_y_ - current_y)));

      Real ideal_diff = d * (current_y - info.ideal_y_);
      Real ideal_score = shrink_extra_weight (ideal_diff, 1.5);

      /* We introduce a power, to make the scoring strictly
         convex. Otherwise a symmetric knee beam (up/down/up/down)
         does not have an optimum in the middle. */
      if (is_knee_)
        ideal_score = pow (ideal_score, 1.1);

      score[d] += length_pen * ideal_score;
      count[d]++;
    }

  /* Divide by number of stems, to make the measure scale-free. */
  Direction d = DOWN;
  do
    score[d] /= max (count[d], 1);
  while (flip (&d) != DOWN);

  config->add (score[LEFT] + score[RIGHT], "L");
}

void
Beam_scoring_problem::score_slope_direction (Beam_configuration *config) const
{
  Real dy = config->y.delta ();
  Real damped_dy = unquanted_y_.delta ();
  Real dem = 0.0;
  /*
    DAMPING_DIRECTION_PENALTY is a very harsh measure, while for
    complex beaming patterns, horizontal is often a good choice.

    TODO: find a way to incorporate the complexity of the beam in this
    penalty.
  */
  if (sign (damped_dy) != sign (dy))
    {
      if (!dy)
        {
          if (fabs (damped_dy / x_span_) > parameters_.ROUND_TO_ZERO_SLOPE)
            dem += parameters_.DAMPING_DIRECTION_PENALTY;
          else
            dem += parameters_.HINT_DIRECTION_PENALTY;
        }
      else
        dem += parameters_.DAMPING_DIRECTION_PENALTY;
    }

  config->add (dem, "Sd");
}

// Score for going against the direction of the musical pattern
void
Beam_scoring_problem::score_slope_musical (Beam_configuration *config) const
{
  Real dy = config->y.delta ();
  Real dem = parameters_.MUSICAL_DIRECTION_FACTOR
             * max (0.0, (fabs (dy) - fabs (musical_dy_)));
  config->add (dem, "Sm");
}

// Score deviation from calculated ideal slope.
void
Beam_scoring_problem::score_slope_ideal (Beam_configuration *config) const
{
  Real dy = config->y.delta ();
  Real damped_dy = unquanted_y_.delta ();
  Real dem = 0.0;

  Real slope_penalty = parameters_.IDEAL_SLOPE_FACTOR;

  /* Xstaff beams tend to use extreme slopes to get short stems. We
     put in a penalty here. */
  if (is_xstaff_)
    slope_penalty *= 10;

  /* Huh, why would a too steep beam be better than a too flat one ? */
  dem += shrink_extra_weight (fabs (damped_dy) - fabs (dy), 1.5)
         * slope_penalty;

  config->add (dem, "Si");
}

static Real
my_modf (Real x)
{
  return x - floor (x);
}

// TODO - there is some overlap with forbidden quants, but for
// horizontal beams, it is much more serious to have stafflines
// appearing in the wrong place, so we have a separate scorer.
void
Beam_scoring_problem::score_horizontal_inter_quants (Beam_configuration *config) const
{
  if (config->y.delta () == 0.0
      && abs (config->y[LEFT]) < staff_radius_ * staff_space_)
    {
      Real yshift = config->y[LEFT] - 0.5 * staff_space_;
      if (fabs (my_round (yshift) - yshift) < 0.01 * staff_space_)
        config->add (parameters_.HORIZONTAL_INTER_QUANT_PENALTY, "H");
    }
}

/*
  TODO: The fixed value SECONDARY_BEAM_DEMERIT is probably flawed:
  because for 32nd and 64th beams the forbidden quants are relatively
  more important than stem lengths.
*/
void
Beam_scoring_problem::score_forbidden_quants (Beam_configuration *config) const
{
  Real dy = config->y.delta ();

  Real extra_demerit = parameters_.SECONDARY_BEAM_DEMERIT
                       / max (edge_beam_counts_[LEFT], edge_beam_counts_[RIGHT]);

  Direction d = LEFT;
  Real dem = 0.0;
  Real eps = parameters_.BEAM_EPS;

  do
    {
      for (int j = 1; j <= edge_beam_counts_[d]; j++)
        {
          Direction stem_dir = edge_dirs_[d];

          /*
            The 2.2 factor is to provide a little leniency for
            borderline cases. If we do 2.0, then the upper outer line
            will be in the gap of the (2, sit) quant, leading to a
            false demerit.
          */
          Real gap1 = config->y[d] - stem_dir * ((j - 1) * beam_translation_ + beam_thickness_ / 2 - line_thickness_ / 2.2);
          Real gap2 = config->y[d] - stem_dir * (j * beam_translation_ - beam_thickness_ / 2 + line_thickness_ / 2.2);

          Interval gap;
          gap.add_point (gap1);
          gap.add_point (gap2);

          for (Real k = -staff_radius_;
               k <= staff_radius_ + eps; k += 1.0)
            if (gap.contains (k))
              {
                Real dist = min (fabs (gap[UP] - k), fabs (gap[DOWN] - k));

                /*
                  this parameter is tuned to grace-stem-length.ly
                */
                Real fixed_demerit = 0.4;

                dem += extra_demerit
                       * (fixed_demerit
                          + (1 - fixed_demerit) * (dist / gap.length ()) * 2);
              }
        }
    }
  while ((flip (&d)) != LEFT);

  if (max (edge_beam_counts_[LEFT], edge_beam_counts_[RIGHT]) >= 2)
    {
      Real straddle = 0.0;
      Real sit = (beam_thickness_ - line_thickness_) / 2;
      Real inter = 0.5;
      Real hang = 1.0 - (beam_thickness_ - line_thickness_) / 2;

      Direction d = LEFT;
      do
        {
          if (edge_beam_counts_[d] >= 2
              && fabs (config->y[d] - edge_dirs_[d] * beam_translation_) < staff_radius_ + inter)
            {
              // TODO up/down symmetry.
              if (edge_dirs_[d] == UP && dy <= eps
                  && fabs (my_modf (config->y[d]) - sit) < eps)
                dem += extra_demerit;

              if (edge_dirs_[d] == DOWN && dy >= eps
                  && fabs (my_modf (config->y[d]) - hang) < eps)
                dem += extra_demerit;
            }

          if (edge_beam_counts_[d] >= 3
              && fabs (config->y[d] - 2 * edge_dirs_[d] * beam_translation_) < staff_radius_ + inter)
            {
              // TODO up/down symmetry.
              if (edge_dirs_[d] == UP && dy <= eps
                  && fabs (my_modf (config->y[d]) - straddle) < eps)
                dem += extra_demerit;

              if (edge_dirs_[d] == DOWN && dy >= eps
                  && fabs (my_modf (config->y[d]) - straddle) < eps)
                dem += extra_demerit;
            }
        }
      while (flip (&d) != LEFT);
    }

  config->add (dem, "F");
}

void
Beam_scoring_problem::score_collisions (Beam_configuration *config) const
{
  Real demerits = 0.0;
  for (vsize i = 0; i < collisions_.size (); i++)
    {
      Interval collision_y = collisions_[i].y_;
      Real x = collisions_[i].x_;

      Real center_beam_y = y_at (x, config);
      Interval beam_y = center_beam_y + collisions_[i].beam_y_;

      Real dist = infinity_f;
      if (!intersection (beam_y, collision_y).is_empty ())
        dist = 0.0;
      else
        dist = min (beam_y.distance (collision_y[DOWN]),
                    beam_y.distance (collision_y[UP]));

      Real scale_free
        = max (parameters_.COLLISION_PADDING - dist, 0.0) /
          parameters_.COLLISION_PADDING;
      demerits
      += collisions_[i].base_penalty_ *
         pow (scale_free, 3) * parameters_.COLLISION_PENALTY;
    }

  config->add (demerits, "C");
}
