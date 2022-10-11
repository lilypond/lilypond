/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "beaming-pattern.hh"
#include "misc.hh"

#include <cstdint>

using std::vector;

/*
  Represents a stem belonging to a beam. Sometimes (for example, if the stem
  belongs to a rest and stemlets aren't used) the stem will be invisible.

  The rhythmic_importance_ of an element tells us the significance of the
  moment at which this element occurs. For example, an element that occurs at
  a beat is more significant than one that doesn't. Smaller number are
  more important. The rhythmic_importance_ is decided and filled in by
  Beaming_pattern. A rhythmic_importance_ smaller than zero has extra
  significance: it represents the start of a beat and therefore beams may
  need to be subdivided.
*/
Beam_rhythmic_element::Beam_rhythmic_element ()
{
  start_moment_ = 0;
  rhythmic_importance_ = 0;
  beam_count_drul_[LEFT] = 0;
  beam_count_drul_[RIGHT] = 0;
  invisible_ = false;
  factor_ = Rational (1);
  tuplet_start_ = false;
}

Beam_rhythmic_element::Beam_rhythmic_element (Rational m, int i, bool inv,
                                              Rational factor,
                                              bool tuplet_start)
{
  start_moment_ = m;
  rhythmic_importance_ = 0;
  beam_count_drul_[LEFT] = i;
  beam_count_drul_[RIGHT] = i;
  invisible_ = inv;
  factor_ = factor;
  tuplet_start_ = tuplet_start;
}

int
Beam_rhythmic_element::count (Direction d) const
{
  return beam_count_drul_[d];
}

/*
  Finds the appropriate direction for the flags at the given index that
  hang below the neighbouring flags. If
  the stem has no more flags than either of its neighbours, this returns
  CENTER.
*/
Direction
Beaming_pattern::flag_direction (Beaming_options const &options, vsize i) const
{
  // The extremal stems shouldn't be messed with, so it's appropriate to
  // return CENTER here also.
  if (i == 0 || i == infos_.size () - 1)
    return CENTER;

  int count
    = infos_[i].count (LEFT); // Both directions should still be the same
  int left_count = infos_[i - 1].count (RIGHT);
  int right_count = infos_[i + 1].count (LEFT);

  // If we are told to subdivide beams and we are next to a beat, point the
  // beamlet away from the beat.
  if (options.subdivide_beams_)
    {
      if (infos_[i].rhythmic_importance_ < 0)
        return RIGHT;
      else if (infos_[i + 1].rhythmic_importance_ < 0)
        return LEFT;
    }

  if (count <= left_count && count <= right_count)
    return CENTER;
  else if (!options.strict_beat_beaming_)
    {
      // Try to avoid sticking-out flags as much as possible by pointing
      // my flags at the neighbor with the most flags.
      if (right_count > left_count)
        return RIGHT;
      else if (left_count > right_count)
        return LEFT;
    }

  // If all else fails, point the beamlet away from the important moment.
  return (infos_[i].rhythmic_importance_ < infos_[i + 1].rhythmic_importance_)
           ? RIGHT
           : LEFT;
}

void
Beaming_pattern::beamify (Beaming_options const &options)
{
  if (infos_.size () <= 1)
    return;

  unbeam_invisible_stems ();

  if (infos_[0].start_moment_ < 0)
    for (vsize i = 0; i < infos_.size (); i++)
      infos_[i].start_moment_ += options.measure_length_;

  find_rhythmic_importance (options);

  vector<Direction> flag_directions;
  // Get the initial flag directions
  for (vsize i = 0; i < infos_.size (); i++)
    flag_directions.push_back (flag_direction (options, i));

  // Correct flag directions for subdivision
  for (vsize i = 1; i < infos_.size () - 1; i++)
    {
      if ((flag_directions[i] == CENTER) && (flag_directions[i - 1] == LEFT))
        flag_directions[i] = RIGHT;
      if ((flag_directions[i] == CENTER) && (flag_directions[i + 1] == RIGHT))
        flag_directions[i] = LEFT;
    }

  // Set the count on each side of the stem
  // We need to run this code twice to make both the
  // left and the right counts work properly
  for (int i = 0; i < 2; i++)
    for (vsize i = 1; i < infos_.size () - 1; i++)
      {
        Direction non_flag_dir = -flag_directions[i];
        if (non_flag_dir)
          {
            int count
              = (infos_[i + 1].rhythmic_importance_ < 0
                 && options.subdivide_beams_)
                  // we're left of a subdivision
                  ? (i != infos_.size () - 2)
                      // respect the beam count for shortened beams ...
                      ? std::max (
                        beam_count_for_rhythmic_position (i + 1),
                        beam_count_for_length (remaining_length (i + 1)))
                      // ... except if there's only one trailing stem
                      : beam_count_for_rhythmic_position (i + 1)

                  // we're at any other stem
                  : std::min (
                    std::min (infos_[i].count (non_flag_dir),
                              infos_[i + non_flag_dir].count (-non_flag_dir)),
                    infos_[i - non_flag_dir].count (non_flag_dir));

            // Ensure at least one beam is left, even for groups longer than 1/8
            count = std::max (count, 1);

            infos_[i].beam_count_drul_[non_flag_dir] = count;
          }
      }
}

/*
   Set the tuplet start moment as necessary
*/
void
update_tuplet (const Rational &start_moment_main, const Rational &factor,
               Rational *tuplet_start)
{
  const auto tuplet_number = factor.den ();
  if ((tuplet_number > 1) && (*tuplet_start < 0))
    *tuplet_start = start_moment_main;
  else if (tuplet_number == 1)
    *tuplet_start = -1;
}

/*
   Get the group start position, the next group starting position, and the
   next beat starting position, given start_moment, base_moment,
   grouping, and factor
*/
void
find_location (SCM grouping, Rational base_moment, Rational start_moment,
               Rational factor, Rational *group_pos, Rational *next_group_pos,
               Rational *next_beat_pos)
{
  *group_pos = {};
  *next_group_pos = {};
  *next_beat_pos = base_moment;

  while (*next_beat_pos <= start_moment)
    *next_beat_pos += base_moment;

  while (*next_group_pos < *next_beat_pos)
    {
      int64_t group_count = 1; //default -- 1 base moments in a beam
      if (scm_is_pair (grouping))
        {
          group_count = from_scm<int> (scm_car (grouping));
          grouping = scm_cdr (grouping);
        }

      // If we have a tuplet, the count should be determined from
      // the maximum tuplet size for beamed tuplets.
      uint64_t tuplet_number = factor.den ();
      if (tuplet_number > 1U)
        {
          // We use 1/8 as the base moment for the tuplet because it's
          // the largest beamed value.  If the tuplet is shorter, it's
          // OK, the code still works
          auto test_count = ((Rational (1, 8) / factor) / base_moment).num ();
          if (test_count > group_count)
            group_count = test_count;
        }
      *group_pos = *next_group_pos;
      *next_group_pos = *group_pos + group_count * base_moment;
    }
}

void
Beaming_pattern::find_rhythmic_importance (Beaming_options const &options)
{
  Rational group_pos; // 0 is the start of the first group
  Rational next_group_pos;
  Rational next_beat_pos (options.base_moment_);
  Rational tuplet_start = -1;
  int64_t tuplet_number = 1;

  SCM grouping = options.grouping_;
  vsize i = 0;

  // Find where we are in the beat structure of the measure
  if (infos_.size ())
    find_location (grouping, options.base_moment_, infos_[i].start_moment_,
                   infos_[i].factor_, &group_pos, &next_group_pos,
                   &next_beat_pos);

  // Mark the importance of stems that start at a beat or a beat group.
  while (i < infos_.size ())
    {
      if ((next_beat_pos > next_group_pos)
          || (infos_[i].start_moment_ > next_beat_pos))
        // Find the new group ending point
        find_location (grouping, options.base_moment_, infos_[i].start_moment_,
                       infos_[i].factor_, &group_pos, &next_group_pos,
                       &next_beat_pos);
      // Mark the start of this beat group
      if (infos_[i].start_moment_ == group_pos)
        infos_[i].rhythmic_importance_ = -2;
      // Work through the end of the beat group or the end of the beam
      while (i < infos_.size () && infos_[i].start_moment_ < next_group_pos)
        {
          // Set the tuplet start as necessary
          update_tuplet (infos_[i].start_moment_, infos_[i].factor_,
                         &tuplet_start);
          const auto dt = infos_[i].start_moment_ - group_pos;
          const auto &tuplet = infos_[i].factor_;
          const auto tuplet_dt = infos_[i].start_moment_ - tuplet_start;
          tuplet_number = tuplet.den ();
          // set the beat end and increment the next beat
          if (infos_[i].start_moment_ == next_beat_pos)
            {
              infos_[i].rhythmic_importance_ = -1;
              next_beat_pos += options.base_moment_;
            }
          // The rhythmic importance of a stem between beats depends on its fraction
          // of a beat: those stems with a lower denominator are deemed more
          // important.  For tuplets, we need to make sure that we use
          // the fraction of the tuplet, instead of the fraction of
          // a beat.
          const auto ratio = (tuplet_number == 1)
                               ? dt / options.base_moment_
                               : tuplet_dt / Rational (1, 8) / tuplet;
          if (infos_[i].rhythmic_importance_ >= 0)
            infos_[i].rhythmic_importance_ = static_cast<int> (ratio.den ());

          i++;
        }

      if (i < infos_.size () && infos_[i].start_moment_ == next_beat_pos)
        {
          if (tuplet_number == 1)
            infos_[i].rhythmic_importance_ = -1;
          next_beat_pos += options.base_moment_;
          if (infos_[i].start_moment_ == next_group_pos)
            infos_[i].rhythmic_importance_ = -2;
        }
    }
}

/*
  Invisible stems should be treated as though they have the same number of
  beams as their least-beamed neighbour. Here we go through the stems and
  modify the invisible stems to satisfy this requirement.
*/
void
Beaming_pattern::unbeam_invisible_stems ()
{
  for (vsize i = 1; i < infos_.size (); i++)
    if (infos_[i].invisible_)
      {
        int b = std::min (infos_[i].count (LEFT), infos_[i - 1].count (LEFT));
        infos_[i].beam_count_drul_[LEFT] = b;
        infos_[i].beam_count_drul_[RIGHT] = b;
      }

  if (infos_.size () > 1)
    for (vsize i = infos_.size () - 1; i--;)
      if (infos_[i].invisible_)
        {
          int b = std::min (infos_[i].count (LEFT), infos_[i + 1].count (LEFT));
          infos_[i].beam_count_drul_[LEFT] = b;
          infos_[i].beam_count_drul_[RIGHT] = b;
        }
}

void
Beaming_pattern::add_stem (Rational m, int b, bool invisible, Rational factor,
                           bool tuplet_start)
{
  infos_.push_back (
    Beam_rhythmic_element (m, b, invisible, factor, tuplet_start));
}

Beaming_pattern::Beaming_pattern ()
{
}

int
Beaming_pattern::beamlet_count (vsize i, Direction d) const
{
  return infos_.at (i).beam_count_drul_[d];
}

Rational
Beaming_pattern::start_moment (vsize i) const
{
  return infos_.at (i).start_moment_;
}

Rational
Beaming_pattern::end_moment (vsize i) const
{
  Duration dur (
    2 + std::max (beamlet_count (i, LEFT), beamlet_count (i, RIGHT)), 0);

  return infos_.at (i).start_moment_
         + infos_.at (i).factor_ * dur.get_length ();
}

Rational
Beaming_pattern::remaining_length (vsize i) const
{
  return end_moment (infos_.size () - 1) - infos_[i].start_moment_;
}

int
Beaming_pattern::beam_count_for_rhythmic_position (vsize idx) const
{
  // Calculate number of beams representing the rhythmic position of given stem
  return intlog2 (infos_[idx].start_moment_.den ()) - 2;
}

int
Beaming_pattern::beam_count_for_length (Rational len) const
{
  return intlog2 (len.den ()) - 2 - intlog2 (len.num ());
}

bool
Beaming_pattern::invisibility (vsize i) const
{
  return infos_.at (i).invisible_;
}

Rational
Beaming_pattern::factor (vsize i) const
{
  return infos_.at (i).factor_;
}

bool
Beaming_pattern::tuplet_start (vsize i) const
{
  return infos_.at (i).tuplet_start_;
}

/*
    Split a beaming pattern at index i and return a new
    Beaming_pattern containing the removed elements
*/
Beaming_pattern *
Beaming_pattern::split_pattern (vsize i)
{
  Beaming_pattern *new_pattern = 0;
  int count;

  new_pattern = new Beaming_pattern ();
  for (vsize j = i + 1; j < infos_.size (); j++)
    {
      count = std::max (beamlet_count (j, LEFT), beamlet_count (j, RIGHT));
      new_pattern->add_stem (start_moment (j), count, invisibility (j),
                             factor (j), tuplet_start (j));
    }
  for (vsize j = i + 1; j < infos_.size ();)
    infos_.pop_back ();
  return (new_pattern);
}

void
Beaming_options::from_context (Context *context)
{
  grouping_ = get_property (context, "beatStructure");
  subdivide_beams_ = from_scm<bool> (get_property (context, "subdivideBeams"));
  strict_beat_beaming_
    = from_scm<bool> (get_property (context, "strictBeatBeaming"));
  base_moment_
    = from_scm (get_property (context, "baseMoment"), Moment (1, 4)).main_part_;
  measure_length_
    = from_scm (get_property (context, "measureLength"), Moment (4, 4))
        .main_part_;
}

Beaming_options::Beaming_options ()
{
  grouping_ = SCM_EOL;
  subdivide_beams_ = false;
  strict_beat_beaming_ = false;
}

void
Beaming_options::gc_mark () const
{
  scm_gc_mark (grouping_);
}
