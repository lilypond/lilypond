/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#include "config.hh"

#include "beam-scoring-problem.hh"

#include "align-interface.hh"
#include "beam.hh"
#include "direction.hh"
#include "directional-element-interface.hh"
#include "grob.hh"
#include "grob-array.hh"
#include "item.hh"
#include "international.hh"
#include "interval-minefield.hh"
#include "least-squares.hh"
#include "libc-extension.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stencil.hh"
#include "stem.hh"
#include "warn.hh"
#include "string-convert.hh"

#include <algorithm>
#include <cmath>
#include <memory>
#include <queue>
#include <set>
#include <vector>

using std::set;
using std::string;
using std::unique_ptr;
using std::vector;

// Compute the increase from dr.front () to dr.back ().
static constexpr Real
delta (const Drul_array<Real> &dr)
{
  return dr.back () - dr.front ();
}

Real
get_detail (SCM alist, SCM sym, Real def)
{
  SCM entry = scm_assq (sym, alist);

  if (scm_is_pair (entry))
    return from_scm<double> (scm_cdr (entry), def);
  return def;
}

void
Beam_quant_parameters::fill (Grob *him)
{
  SCM details = get_property (him, "details");

  // General
  BEAM_EPS = get_detail (details, ly_symbol2scm ("beam-eps"), 1e-3);
  REGION_SIZE = get_detail (details, ly_symbol2scm ("region-size"), 2);

  // forbidden quants
  SECONDARY_BEAM_DEMERIT
    = get_detail (details, ly_symbol2scm ("secondary-beam-demerit"), 10.0)
      // For stems that are non-standard, the forbidden beam quanting
      // doesn't really work, so decrease their importance.
      * exp (
        -8
        * fabs (
          1.0 - from_scm<double> (get_property (him, "length-fraction"), 1.0)));
  STEM_LENGTH_DEMERIT_FACTOR
    = get_detail (details, ly_symbol2scm ("stem-length-demerit-factor"), 5);
  HORIZONTAL_INTER_QUANT_PENALTY
    = get_detail (details, ly_symbol2scm ("horizontal-inter-quant"), 500);

  STEM_LENGTH_LIMIT_PENALTY
    = get_detail (details, ly_symbol2scm ("stem-length-limit-penalty"), 5000);
  DAMPING_DIRECTION_PENALTY
    = get_detail (details, ly_symbol2scm ("damping-direction-penalty"), 800);
  HINT_DIRECTION_PENALTY
    = get_detail (details, ly_symbol2scm ("hint-direction-penalty"), 20);
  MUSICAL_DIRECTION_FACTOR
    = get_detail (details, ly_symbol2scm ("musical-direction-factor"), 400);
  IDEAL_SLOPE_FACTOR
    = get_detail (details, ly_symbol2scm ("ideal-slope-factor"), 10);
  ROUND_TO_ZERO_SLOPE
    = get_detail (details, ly_symbol2scm ("round-to-zero-slope"), 0.02);

  // Collisions
  COLLISION_PENALTY
    = get_detail (details, ly_symbol2scm ("collision-penalty"), 500);

  /* For grace notes, beams get scaled down to 80%, but glyphs go down
     to 63% (magstep -4 for accidentals). To make the padding
     commensurate with glyph size for grace notes, we take the square
     of the length fraction, yielding a 64% decrease.
   */
  COLLISION_PADDING
    = get_detail (details, ly_symbol2scm ("collision-padding"), 0.5)
      * sqr (from_scm<double> (get_property (him, "length-fraction"), 1.0));
  STEM_COLLISION_FACTOR
    = get_detail (details, ly_symbol2scm ("stem-collision-factor"), 0.1);
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
  demerits = 0.0;
  next_scorer_todo = ORIGINAL_DISTANCE;
}

bool
Beam_configuration::done () const
{
  return next_scorer_todo >= NUM_SCORERS;
}

void
Beam_configuration::add (Real demerit, const string &reason)
{
  demerits += demerit;

  if (demerit)
    score_card_ += to_string (" %s %.2f", reason.c_str (), demerit);
}

unique_ptr<Beam_configuration>
Beam_configuration::new_config (Drul_array<Real> start, Drul_array<Real> offset)
{
  auto qs = std::make_unique<Beam_configuration> ();
  qs->y = Drul_array<Real> (int (start[LEFT]) + offset[LEFT],
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
  return p->y[LEFT] + x * delta (p->y) / x_span_;
}

/****************************************************************/

/*
  TODO:

  - Make all demerits customisable

  - Add demerits for quants per se, as to forbid a specific quant
  entirely
*/

void
Beam_scoring_problem::add_collision (Real x, Interval y, Real score_factor)
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

void
Beam_scoring_problem::init_instance_variables (Grob *me, Drul_array<Real> ys,
                                               bool align_broken_intos)
{
  beam_ = dynamic_cast<Spanner *> (me);
  unquanted_y_ = ys;

  /*
    If 'ys' are finite, use them as starting points for y-positions of the
    ends of the beam, instead of the best-fit through the natural ends of
    the stems.  Otherwise, we want to do initial slope calculations.
  */
  do_initial_slope_calculations_ = false;
  for (const auto d : {LEFT, RIGHT})
    do_initial_slope_calculations_ |= !std::isfinite (unquanted_y_[d]);

  /*
    Calculations are relative to a unit-scaled staff, i.e. the quants are
    divided by the current staff_space_.
  */
  staff_space_ = Staff_symbol_referencer::staff_space (beam_);
  beam_thickness_ = Beam::get_beam_thickness (beam_) / staff_space_;
  line_thickness_
    = Staff_symbol_referencer::line_thickness (beam_) / staff_space_;
  max_beam_count_ = Beam::get_beam_count (beam_);
  length_fraction_
    = from_scm<double> (get_property (beam_, "length-fraction"), 1.0);
  // This is the least-squares DY, corrected for concave beams.
  musical_dy_ = from_scm<double> (get_property (beam_, "least-squares-dy"), 0);

  vector<Spanner *> beams;
  align_broken_intos_ = align_broken_intos;
  if (align_broken_intos_)
    {
      Spanner *orig = beam_->original ();
      if (!orig)
        align_broken_intos_ = false;
      else if (!orig->broken_intos_.size ())
        align_broken_intos_ = false;
      else
        beams.insert (beams.end (), orig->broken_intos_.begin (),
                      orig->broken_intos_.end ());
    }
  if (!align_broken_intos_)
    beams.push_back (beam_);

  /*
    x_span_ is a single scalar, cumulatively summing the length of all the
    segments the parent beam was broken-into.
  */
  x_span_ = 0.0;
  is_knee_ = false;
  normal_stem_count_ = 0;
  for (vsize i = 0; i < beams.size (); i++)
    {
      extract_grob_set (beams[i], "stems", stems);
      extract_grob_set (beams[i], "covered-grobs", fake_collisions);
      vector<Grob *> collisions;

      for (vsize j = 0; j < fake_collisions.size (); j++)
        if (fake_collisions[j]->get_system () == beams[i]->get_system ())
          collisions.push_back (fake_collisions[j]);

      Grob *common[NO_AXES];
      for (const auto a : {X_AXIS, Y_AXIS})
        common[a] = common_refpoint_of_array (stems, beams[i], a);

      for (const auto d : {LEFT, RIGHT})
        common[X_AXIS]
          = beams[i]->get_bound (d)->common_refpoint (common[X_AXIS], X_AXIS);

      // positions of the endpoints of this beam segment, including any overhangs
      const Interval x_pos = from_scm (get_property (beams[i], "X-positions"),
                                       Interval (0.0, 0.0));

      Drul_array<Grob *> edge_stems (Beam::first_normal_stem (beams[i]),
                                     Beam::last_normal_stem (beams[i]));

      Drul_array<bool> dirs_found;

      Real my_y = beams[i]->relative_coordinate (common[Y_AXIS], Y_AXIS);

      Interval beam_width (-1.0, -1.0);
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

          Beam_stem_end stem_end
            = Beam::calc_stem_y (beams[i], s, common, x_pos[LEFT], x_pos[RIGHT],
                                 CENTER, Interval (0), 0);
          Real y = stem_end.stem_y_;
          /* Remark:  French Beaming is irrelevant for beam quanting */
          base_lengths_.push_back (y / staff_space_);
          stem_xpositions_.push_back (
            s->relative_coordinate (common[X_AXIS], X_AXIS) - x_pos[LEFT]
            + x_span_);
          stem_ypositions_.push_back (
            s->relative_coordinate (common[Y_AXIS], Y_AXIS) - my_y);

          if (is_normal_.back ())
            {
              if (beam_width[LEFT] == -1.0)
                beam_width[LEFT] = stem_xpositions_.back ();
              beam_width[RIGHT] = stem_xpositions_.back ();
            }
        }

      edge_dirs_ = {};
      normal_stem_count_ += Beam::normal_stem_count (beams[i]);
      if (normal_stem_count_)
        edge_dirs_ = Drul_array<Direction> (stem_infos_[0].dir_,
                                            stem_infos_.back ().dir_);

      is_xstaff_ = has_interface<Align_interface> (common[Y_AXIS]);
      is_knee_ |= dirs_found[DOWN] && dirs_found[UP];

      staff_radius_ = Staff_symbol_referencer::staff_radius (beams[i]);
      edge_beam_counts_ = Drul_array<int> (
        Stem::beam_multiplicity (stems[0]).length () + 1,
        Stem::beam_multiplicity (stems.back ()).length () + 1);

      // TODO - why are we dividing by staff_space_?
      beam_translation_ = Beam::get_beam_translation (beams[i]) / staff_space_;

      for (const auto d : {LEFT, RIGHT})
        {
          quant_range_[d].set_full ();
          if (!edge_stems[d])
            continue;

          Real stem_offset
            = edge_stems[d]->relative_coordinate (common[Y_AXIS], Y_AXIS)
              - beams[i]->relative_coordinate (common[Y_AXIS], Y_AXIS);
          Interval heads
            = Stem::head_positions (edge_stems[d]) * 0.5 * staff_space_;

          Direction ed = edge_dirs_[d];
          heads.widen (0.5 * staff_space_
                       + (edge_beam_counts_[d] - 1) * beam_translation_
                       + beam_thickness_ * .5);
          quant_range_[d][-ed] = heads[ed] + stem_offset;
        }

      segments_ = Beam::get_beam_segments (beams[i]);
      std::sort (segments_.begin (), segments_.end (), beam_segment_less);
      for (vsize j = 0; j < segments_.size (); j++)
        segments_[j].horizontal_ += (x_span_ - x_pos[LEFT]);

      set<Grob *> colliding_stems;
      for (vsize j = 0; j < collisions.size (); j++)
        {
          if (!collisions[j]->is_live ())
            continue;

          if (has_interface<Beam> (collisions[j])
              && Beam::is_cross_staff (collisions[j]))
            continue;

          Box b;
          for (const auto a : {X_AXIS, Y_AXIS})
            b[a] = collisions[j]->extent (common[a], a);

          if (b[X_AXIS][RIGHT] < x_pos[LEFT] || b[X_AXIS][LEFT] > x_pos[RIGHT])
            continue;
          if (b[X_AXIS].is_empty () || b[Y_AXIS].is_empty ())
            continue;

          b[X_AXIS] += (x_span_ - x_pos[LEFT]);
          b[Y_AXIS] -= my_y;
          Real width = b[X_AXIS].length ();
          Real width_factor = sqrt (width / staff_space_);

          for (const auto d : {LEFT, RIGHT})
            add_collision (b[X_AXIS][d], b[Y_AXIS], width_factor);

          Grob *stem = unsmob<Grob> (get_object (collisions[j], "stem"));
          if (has_interface<Stem> (stem) && Stem::is_normal_stem (stem))
            {
              colliding_stems.insert (stem);
            }
        }

      for (set<Grob *>::const_iterator it (colliding_stems.begin ());
           it != colliding_stems.end (); it++)
        {
          Grob *s = *it;
          Real x = (robust_relative_extent (s, common[X_AXIS], X_AXIS)
                    - x_pos[LEFT] + x_span_)
                     .center ();

          Direction stem_dir = get_grob_direction (*it);
          Interval y;
          y.set_full ();
          y[-stem_dir] = Stem::chord_start_y (*it)
                         + (*it)->relative_coordinate (common[Y_AXIS], Y_AXIS)
                         - my_y;

          Real factor = parameters_.STEM_COLLISION_FACTOR;
          if (!unsmob<Grob> (get_object (s, "beam")))
            factor = 1.0;
          add_collision (x, y, factor);
        }
      x_span_ += beams[i]->spanner_length ();
    }
}

Beam_scoring_problem::Beam_scoring_problem (Grob *me, Drul_array<Real> ys,
                                            bool align_broken_intos)
{
  beam_ = dynamic_cast<Spanner *> (me);
  unquanted_y_ = ys;
  align_broken_intos_ = align_broken_intos;

  parameters_.fill (me);
  init_instance_variables (me, ys, align_broken_intos);
  if (do_initial_slope_calculations_)
    {
      least_squares_positions ();
      slope_damping ();
      shift_region_to_valid ();
    }
}

// Assuming V is not empty, pick a 'reasonable' point inside V.
static Real
point_in_interval (Interval v, Real dist)
{
  if (std::isinf (v[DOWN]))
    return v[UP] - dist;
  else if (std::isinf (v[UP]))
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

      *dy = sign (*dy)
            * std::max (fabs (*dy), std::min (std::min (sit, inter), hang));
    }
}

void
Beam_scoring_problem::no_visible_stem_positions ()
{
  if (!head_positions_.size ())
    {
      unquanted_y_ = {};
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

  Real y = head_positions.linear_combination (dir) * 0.5 * staff_space_
           + dir * beam_translation_ * (multiplicity.length () + 1);

  unquanted_y_ = Drul_array<Real> (y, y);
}

vsize
Beam_scoring_problem::first_normal_index ()
{
  for (vsize i = 0; i < is_normal_.size (); i++)
    if (is_normal_[i])
      return i;

  beam_->programming_error (
    "No normal stems, but asking for first normal stem index.");
  return 0;
}

vsize
Beam_scoring_problem::last_normal_index ()
{
  for (vsize i = is_normal_.size (); i--;)
    if (is_normal_[i])
      return i;

  beam_->programming_error (
    "No normal stems, but asking for first normal stem index.");
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

  Drul_array<Real> ideal (stem_infos_[fnx].ideal_y_ + stem_ypositions_[fnx],
                          stem_infos_[lnx].ideal_y_ + stem_ypositions_[lnx]);

  Real y = 0;
  Real slope = 0;
  Real dy = 0;
  Real ldy = 0.0;
  if (!delta (ideal))
    {
      Drul_array<Real> chord (chord_start_y_[0], chord_start_y_.back ());

      /* Simple beams (2 stems) on middle line should be allowed to be
         slightly sloped.

         However, if both stems reach middle line,
         ideal[LEFT] == ideal[RIGHT] and delta (ideal) == 0.

         For that case, we apply artificial slope */
      if (!ideal[LEFT] && delta (chord) && stem_infos_.size () == 2)
        {
          const Direction d (delta (chord));
          unquanted_y_[d] = Beam::get_beam_thickness (beam_) / 2;
          unquanted_y_[-d] = -unquanted_y_[d];
        }
      else
        unquanted_y_ = ideal;

      ldy = unquanted_y_[RIGHT] - unquanted_y_[LEFT];
    }
  else
    {
      vector<Offset> ideals;
      for (vsize i = 0; i < stem_infos_.size (); i++)
        if (is_normal_[i])
          ideals.push_back (
            Offset (stem_xpositions_[i],
                    stem_infos_[i].ideal_y_ + stem_ypositions_[i]));

      minimise_least_squares (&slope, &y, ideals);

      dy = slope * x_span_;

      set_minimum_dy (beam_, &dy);

      ldy = dy;
      unquanted_y_ = Drul_array<Real> (y, (y + dy));
    }

  musical_dy_ = ldy;
  set_property (beam_, "least-squares-dy", to_scm (musical_dy_));
}

/*
  Determine whether a beam is concave.

  A beam is concave when the middle notes get closer to the
  beam than the left and right edge notes.

  This is determined in two ways: by looking at the positions of the
  middle notes, or by looking at the deviation of the inside notes
  compared to the line connecting first and last.

  The tricky thing is what to do with beams with chords. There are no
  real guidelines in this case.
*/

bool
is_concave_single_notes (vector<int> const &positions, Direction beam_dir)
{
  Interval covering;
  covering.add_point (positions[0]);
  covering.add_point (positions.back ());

  bool above = false;
  bool below = false;
  bool concave = false;

  /*
    notes above and below the interval covered by 1st and last note.
  */
  for (vsize i = 1; i + 1 < positions.size (); i++)
    {
      above = above || (positions[i] > covering[UP]);
      below = below || (positions[i] < covering[DOWN]);
    }

  concave = concave || (above && below);
  /*
    A note as close or closer to the beam than begin and end, but the
    note is reached in the opposite direction as the last-first dy
  */
  int dy = positions.back () - positions[0];
  int closest
    = std::max (beam_dir * positions.back (), beam_dir * positions[0]);
  for (vsize i = 2; !concave && i + 1 < positions.size (); i++)
    {
      int inner_dy = positions[i] - positions[i - 1];
      if (sign (inner_dy) != sign (dy)
          && (beam_dir * positions[i] >= closest
              || beam_dir * positions[i - 1] >= closest))
        concave = true;
    }

  bool all_closer = true;
  for (vsize i = 1; all_closer && i + 1 < positions.size (); i++)
    {
      all_closer = all_closer && (beam_dir * positions[i] > closest);
    }

  concave = concave || all_closer;
  return concave;
}

Real
calc_positions_concaveness (vector<int> const &positions, Direction beam_dir)
{
  Real dy = positions.back () - positions[0];
  Real slope = dy / static_cast<Real> (positions.size () - 1);
  Real concaveness = 0.0;
  for (vsize i = 1; i + 1 < positions.size (); i++)
    {
      Real line_y = slope * static_cast<Real> (i) + positions[0];
      concaveness += std::max (beam_dir * (positions[i] - line_y), 0.0);
    }

  concaveness /= static_cast<Real> (positions.size ());

  /*
    Normalize. For dy = 0, the slope ends up as 0 anyway, so the
    scaling of concaveness doesn't matter much.
  */
  if (dy)
    concaveness /= fabs (dy);
  return concaveness;
}

Real
Beam_scoring_problem::calc_concaveness ()
{
  SCM conc = get_property (beam_, "concaveness");
  if (scm_is_number (conc))
    return from_scm<double> (conc);

  if (is_knee_ || is_xstaff_)
    return 0.0;

  Direction beam_dir = CENTER;
  for (vsize i = is_normal_.size (); i--;)
    if (is_normal_[i] && stem_infos_[i].dir_)
      beam_dir = stem_infos_[i].dir_;

  if (normal_stem_count_ <= 2)
    return 0.0;

  vector<int> close_positions;
  vector<int> far_positions;
  for (vsize i = 0; i < is_normal_.size (); i++)
    if (is_normal_[i])
      {
        /*
          For chords, we take the note head that is closest to the beam.

          Hmmm.. wait, for the beams in the last measure of morgenlied,
          this doesn't look so good. Let's try the heads farthest from
          the beam.
        */

        auto close_pos = static_cast<int> (rint (head_positions_[i][beam_dir]));
        close_positions.push_back (close_pos);
        auto far_pos = static_cast<int> (rint (head_positions_[i][-beam_dir]));
        far_positions.push_back (far_pos);
      }

  Real concaveness = 0.0;

  if (is_concave_single_notes (beam_dir == UP ? close_positions : far_positions,
                               beam_dir))
    {
      concaveness = 10000;
    }
  else
    {
      concaveness = (calc_positions_concaveness (far_positions, beam_dir)
                     + calc_positions_concaveness (close_positions, beam_dir))
                    / 2;
    }

  return concaveness;
}

void
Beam_scoring_problem::slope_damping ()
{
  if (normal_stem_count_ <= 1)
    return;

  SCM s = get_property (beam_, "damping");
  Real damping = from_scm<double> (s);
  Real concaveness = calc_concaveness ();
  if ((concaveness >= 10000) || (damping >= 10000))
    {
      unquanted_y_[LEFT] = unquanted_y_[RIGHT];
      musical_dy_ = 0;
      damping = 0;
    }

  if ((damping) && (damping + concaveness))
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
      Real left_y = stem_infos_[i].shortest_y_ - slope * stem_xpositions_[i];

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

      Real dy = slope * collisions_[i].x_;

      Interval disallowed;
      for (const auto yd : {DOWN, UP})
        {
          Real left_y = collisions_[i].y_[yd] - dy;
          disallowed[yd] = left_y;
        }

      forbidden_intervals.push_back (disallowed);
    }

  std::sort (forbidden_intervals.begin (), forbidden_intervals.end (),
             Interval::left_less);
  Real beam_left_y = unquanted_y_[LEFT];
  Interval feasible_beam_placements (beam_left_y, beam_left_y);

  Interval_minefield minefield (feasible_beam_placements, 0.0);
  for (vsize i = 0; i < forbidden_intervals.size (); i++)
    minefield.add_forbidden_interval (forbidden_intervals[i]);
  minefield.solve ();
  feasible_beam_placements = minefield.feasible_placements ();

  // if the beam placement falls out of the feasible region, we push it
  // to infinity so that it can never be a feasible candidate below
  for (const auto d : {DOWN, UP})
    {
      if (!feasible_left_point.contains (feasible_beam_placements[d]))
        feasible_beam_placements[d] = d * infinity_f;
    }

  if ((feasible_beam_placements[UP] == infinity_f
       && feasible_beam_placements[DOWN] == -infinity_f)
      && !feasible_left_point.is_empty ())
    {
      // We are somewhat screwed: we have a collision, but at least
      // there is a way to satisfy stem length constraints.
      beam_left_y = point_in_interval (feasible_left_point, 2.0);
    }
  else if (!feasible_left_point.is_empty ())
    {
      // Only one of them offers is feasible solution. Pick that one.
      if (abs (beam_left_y - feasible_beam_placements[DOWN])
          > abs (beam_left_y - feasible_beam_placements[UP]))
        beam_left_y = feasible_beam_placements[UP];
      else
        beam_left_y = feasible_beam_placements[DOWN];
    }
  else
    {
      // We are completely screwed.
      beam_->warning (_ (
        "no viable initial configuration found: may not find good beam slope"));
    }

  unquanted_y_ = Drul_array<Real> (beam_left_y, (beam_left_y + beam_dy));
}

void
Beam_scoring_problem::generate_quants (
  vector<unique_ptr<Beam_configuration>> *scores) const
{
  auto region_size = static_cast<int> (parameters_.REGION_SIZE);

  // Knees and collisions are harder, lets try some more possibilities
  if (is_knee_)
    region_size += 2;
  if (collisions_.size ())
    region_size += 2;

  Real straddle = 0.0;
  Real sit = (beam_thickness_ - line_thickness_) / 2;
  Real inter = 0.5;
  Real hang = 1.0 - (beam_thickness_ - line_thickness_) / 2;
  Real base_quants[] = {straddle, sit, inter, hang};
  int num_base_quants = int (sizeof (base_quants) / sizeof (Real));

  /* for normal-sized beams, in case of more than 4 beams, the outer beam
     used for generating quants will never interfere with staff lines, but
     prevent the inside-staff beams from being neatly positioned.
     A correctional grid_shift has to be applied to compensate. */
  Real grid_shift = 0.0;
  /* grid shift only makes sense for widened normal-sized beams: */
  if (!is_knee_ && max_beam_count_ > 4 && length_fraction_ == 1.0)
    grid_shift = (max_beam_count_ - 4) * (1.0 - beam_translation_);

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
        Interval corr (0.0, 0.0);
        if (grid_shift)
          for (const auto d : {LEFT, RIGHT})
            /* apply grid shift if quant outside 5-line staff: */
            if ((unquanted_y_[d] + unshifted_quants[i]) * edge_dirs_[d] > 2.5)
              corr[d] = grid_shift * edge_dirs_[d];
        auto c = Beam_configuration::new_config (
          unquanted_y_, Drul_array<Real> (unshifted_quants[i] - corr[LEFT],
                                          unshifted_quants[j] - corr[RIGHT]));

        for (const auto d : {LEFT, RIGHT})
          {
            if (!quant_range_[d].contains (c->y[d]))
              {
                c.reset ();
                break;
              }
          }
        if (c)
          scores->push_back (std::move (c));
      }
}

void
Beam_scoring_problem::one_scorer (Beam_configuration *config) const
{
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
Beam_scoring_problem::force_score (
  SCM inspect_quants,
  const vector<unique_ptr<Beam_configuration>> &configs) const
{
  Drul_array<Real> ins = from_scm<Drul_array<Real>> (inspect_quants);
  Real mindist = 1e6;
  Beam_configuration *best = NULL;
  for (vsize i = 0; i < configs.size (); i++)
    {
      Real d = fabs (configs[i]->y[LEFT] - ins[LEFT])
               + fabs (configs[i]->y[RIGHT] - ins[RIGHT]);
      if (d < mindist)
        {
          best = configs[i].get ();
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
  vector<unique_ptr<Beam_configuration>> configs;
  generate_quants (&configs);

  if (configs.empty ())
    {
      programming_error (
        "No viable beam quanting found.  Using unquanted y value.");
      return unquanted_y_;
    }

  if (from_scm<bool> (get_property (beam_, "skip-quanting")))
    return unquanted_y_;

  Beam_configuration *best = NULL;

  bool debug = from_scm<bool> (
    beam_->layout ()->lookup_variable (ly_symbol2scm ("debug-beam-scoring")));
  SCM inspect_quants = get_property (beam_, "inspect-quants");
  if (scm_is_pair (inspect_quants))
    {
      debug = true;
      best = force_score (inspect_quants, configs);
    }
  else
    {
      std::priority_queue<Beam_configuration *,
                          std::vector<Beam_configuration *>,
                          Beam_configuration_less>
        queue;
      for (vsize i = 0; i < configs.size (); i++)
        queue.push (configs[i].get ());

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

  Drul_array<Real> final_positions = best->y;

  if (debug)
    {
      // debug quanting
      int completed = 0;
      for (vsize i = 0; i < configs.size (); i++)
        {
          if (configs[i]->done ())
            completed++;
        }

      string card = best->score_card_
                    + to_string (" c%d/%zu", completed, configs.size ());
      set_property (beam_, "annotation", ly_string2scm (card));
    }

  configs.clear ();
  if (align_broken_intos_)
    {
      Interval normalized_endpoints = from_scm (
        get_property (beam_, "normalized-endpoints"), Interval (0, 1));
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
  Drul_array<Real> score;
  Drul_array<int> count;

  for (vsize i = 0; i < stem_xpositions_.size (); i++)
    {
      if (!is_normal_[i])
        continue;

      Real x = stem_xpositions_[i];
      Real dx = x_span_;
      Real beam_y
        = dx ? config->y[RIGHT] * x / dx + config->y[LEFT] * (x_span_ - x) / dx
             : (config->y[RIGHT] + config->y[LEFT]) / 2;
      Real current_y = beam_y + base_lengths_[i];
      Real length_pen = parameters_.STEM_LENGTH_DEMERIT_FACTOR;

      Stem_info info = stem_infos_[i];
      Direction d = info.dir_;

      score[d]
        += limit_penalty * std::max (0.0, (d * (info.shortest_y_ - current_y)));

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
  for (const auto d : {DOWN, UP})
    score[d] /= std::max (count[d], 1);

  /*
    sometimes, two perfectly symmetric kneed beams will have the same score
    and can either be quanted up or down.

    we choose the quanting in the direction of the slope so that the first stem
    always seems longer, reaching to the second, rather than squashed.
  */
  if (is_knee_ && (count[LEFT] == count[RIGHT]) && (count[LEFT] == 1))
    {
      const Direction d (delta (unquanted_y_));
      if (d)
        score[d] += (score[d] < 1.0) ? 0.01 : 0.0;
    }

  config->add (score[LEFT] + score[RIGHT], "L");
}

void
Beam_scoring_problem::score_slope_direction (Beam_configuration *config) const
{
  Real dy = delta (config->y);
  Real damped_dy = delta (unquanted_y_);
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
  Real dy = delta (config->y);
  Real dem = parameters_.MUSICAL_DIRECTION_FACTOR
             * std::max (0.0, (fabs (dy) - fabs (musical_dy_)));
  config->add (dem, "Sm");
}

// Score deviation from calculated ideal slope.
void
Beam_scoring_problem::score_slope_ideal (Beam_configuration *config) const
{
  Real dy = delta (config->y);
  Real damped_dy = delta (unquanted_y_);
  Real dem = 0.0;

  Real slope_penalty = parameters_.IDEAL_SLOPE_FACTOR;

  /* Xstaff beams tend to use extreme slopes to get short stems. We
     put in a penalty here. */
  if (is_xstaff_)
    slope_penalty *= 10;

  /* Huh, why would a too steep beam be better than a too flat one ? */
  dem
    += shrink_extra_weight (fabs (damped_dy) - fabs (dy), 1.5) * slope_penalty;

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
Beam_scoring_problem::score_horizontal_inter_quants (
  Beam_configuration *config) const
{
  if (delta (config->y) == 0.0
      && abs (config->y[LEFT]) < staff_radius_ * staff_space_)
    {
      Real yshift = config->y[LEFT] - 0.5 * staff_space_;
      if (fabs (round_halfway_up (yshift) - yshift) < 0.01 * staff_space_)
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
  Real dy = delta (config->y);

  Real extra_demerit
    = parameters_.SECONDARY_BEAM_DEMERIT
      / std::max (edge_beam_counts_[LEFT], edge_beam_counts_[RIGHT]);

  Real dem = 0.0;
  Real eps = parameters_.BEAM_EPS;

  for (const auto d : {LEFT, RIGHT})
    {
      for (int j = 1; j <= edge_beam_counts_[d]; j++)
        {
          Direction stem_dir = edge_dirs_[d];

          /*
            The fudge_factor is to provide a little leniency for
            borderline cases. If we do 2.0, then the upper outer line
            will be in the gap of the (2, sit) quant, leading to a
            false demerit. By increasing the fudge factor to 2.2, we
            fix this case.
          */
          Real fudge_factor = 2.2;
          Real gap1 = config->y[d]
                      - stem_dir
                          * ((j - 1) * beam_translation_ + beam_thickness_ / 2
                             - line_thickness_ / fudge_factor);
          Real gap2 = config->y[d]
                      - stem_dir
                          * (j * beam_translation_ - beam_thickness_ / 2
                             + line_thickness_ / fudge_factor);

          Interval gap;
          gap.add_point (gap1);
          gap.add_point (gap2);

          for (Real k = -staff_radius_; k <= staff_radius_ + eps; k += 1.0)
            if (gap.contains (k))
              {
                Real dist = std::min (fabs (gap[UP] - k), fabs (gap[DOWN] - k));

                /*
                  this parameter is tuned to grace-stem-length.ly
                  retuned from 0.40 to 0.39 by MS because of slight increases
                  in gap.length () resulting from measuring beams at real ends
                  instead of from the middle of stems.

                  TODO:
                  This function needs better comments so we know what is forbidden
                  and why.
                */
                Real fixed_demerit = 0.39;

                dem += extra_demerit
                       * (fixed_demerit
                          + (1 - fixed_demerit) * (dist / gap.length ()) * 2);
              }
        }
    }

  config->add (dem, "Fl");
  dem = 0.0;
  if (std::max (edge_beam_counts_[LEFT], edge_beam_counts_[RIGHT]) >= 2)
    {
      Real straddle = 0.0;
      Real sit = (beam_thickness_ - line_thickness_) / 2;
      Real inter = 0.5;
      Real hang = 1.0 - (beam_thickness_ - line_thickness_) / 2;

      for (const auto d : {LEFT, RIGHT})
        {
          if (edge_beam_counts_[d] >= 2
              && fabs (config->y[d] - edge_dirs_[d] * beam_translation_)
                   < staff_radius_ + inter)
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
              && fabs (config->y[d] - 2 * edge_dirs_[d] * beam_translation_)
                   < staff_radius_ + inter)
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
    }

  config->add (dem, "Fs");
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
        dist = std::min (beam_y.distance (collision_y[DOWN]),
                         beam_y.distance (collision_y[UP]));

      Real scale_free = std::max (parameters_.COLLISION_PADDING - dist, 0.0)
                        / parameters_.COLLISION_PADDING;
      Real collision_demerit = collisions_[i].base_penalty_
                               * pow (scale_free, 3)
                               * parameters_.COLLISION_PENALTY;

      if (collision_demerit > 0)
        {
          demerits += collision_demerit;
        }
    }

  config->add (demerits, "C");
}
