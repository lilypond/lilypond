/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "beaming-pattern.hh"
#include "lily-guile.hh"
#include "lily-guile-macros.hh"
#include "misc.hh"
#include "moment.hh"
#include "stream-event.hh"
#include "tuplet-description.hh"

#include <forward_list>
#include <vector>

/*
  Represents a stem belonging to a beam. Sometimes (for example, if the stem
  belongs to a rest and stemlets aren't used) the stem will be invisible.

  The rhythmic_importance_ of an element tells us the significance of the
  moment at which this element occurs. A naive calculation would be
  the binary logarithm of the denominator of the difference between
  the stem's start position and the first stem's start position, minus 2.
  For examle, if we have consecutive 32nd notes, their rhythmic importance will
  likely be 0 3 2 3 1 3 2 3 0 3 2 3 ...

  Smaller values are more important. The rhythmic_importance_ is decided
  and filled in by Beaming_pattern. The first stem's rhythmic_importance_
  value is theoretically unnecessary for auto-beaming calculations,
  but this is
  explained in set_rhythmic_importance.
*/
Beaming_pattern::Beam_rhythmic_element::Beam_rhythmic_element (
  Rational const &m, bool inv, Duration const &duration,
  Tuplet_description const *tuplet)
  : start_moment_ (m),
    beam_count_ (
      static_cast<unsigned> (std::max (duration.duration_log () - 2, 0))),
    invisible_ (inv),
    duration_ (duration),
    tuplet_ (tuplet)
{
  beam_count_drul_[LEFT] = beam_count_;
  beam_count_drul_[RIGHT] = beam_count_;
}

unsigned
Beaming_pattern::Beam_rhythmic_element::count () const
{
  return beam_count_;
}

static int
rhythmic_importance_for_position (Rational const &r)
{
  return intlog2 (r.den ()) - 2 - (r.den () == 1 ? intlog2 (r.num ()) : 0);
}
static int
rhythmic_importance_for_length (Rational const &r)
{
  return intlog2 (r.den ()) - 2 - intlog2 (r.num ());
}

/*
  For grace beaming, which involves negative stem start moments,
  the measure position needs to undergo modulo to be nonnegative
*/
Beaming_pattern::Beaming_pattern (Rational const &measure_offset)
  : measure_offset_ (measure_offset)
{
  if (measure_offset < 0)
    programming_error ("measure offset should not be negative");
}

/*
  The function to call to set the stems' beamlet counts
*/
void
Beaming_pattern::beamify (Beaming_options const &options)
{
  if (infos_.size () <= 1)
    return;

  unbeam_invisible_stems ();

  set_rhythmic_importance (options);

  {
    std::vector<Direction> flag_directions (infos_.size (), CENTER);
    Rational cur_beat, next_beat = infos_[0].start_moment_ - measure_offset_;
    SCM remaining_beats = SCM_EOL;

    for (vsize i = 1; i < infos_.size () - 1; i++)
      {
        // If we ever allow setting custom stem flag directions that
        // automatic beam subdivision would obey, here would be the place
        // to set the value and 'continue'
        unsigned const left_count = infos_[i - 1].count (),
                       right_count = infos_[i + 1].count ();

        // Stems at boundaries of tuplet spans must have CENTER direction.
        // That's why we don't iterate 0 and infos_.size() - 1 as an optimization.
        if (!at_span_start (i) && !at_span_stop (i)
            && infos_[i].count () > std::min (left_count, right_count))
          {

            while (next_beat <= infos_[i].start_moment_)
              {
                if (scm_is_null (remaining_beats))
                  remaining_beats = options.beat_structure_;

                cur_beat = next_beat;
                next_beat += from_scm<unsigned> (scm_car (remaining_beats))
                             * options.base_moment_;
                remaining_beats = scm_cdr (remaining_beats);
              }

            bool point_right;
            if (!options.strict_beat_beaming_ && left_count != right_count)
              point_right = right_count > left_count;
            else if ((infos_[i].start_moment_ == cur_beat)
                     != (end_moment (i) == next_beat))
              point_right = infos_[i].start_moment_ == cur_beat;
            else
              point_right = infos_[i].rhythmic_importance_
                            < infos_[i + 1].rhythmic_importance_;

            flag_directions[i] = point_right ? RIGHT : LEFT;
          }
      }

    // Correct flag directions for subdivision
    for (vsize i = 1; i < infos_.size () - 1; i++)
      {
        if (flag_directions[i] == CENTER && flag_directions[i - 1] == LEFT)
          flag_directions[i] = RIGHT;
        if (flag_directions[i] == CENTER && flag_directions[i + 1] == RIGHT)
          flag_directions[i] = LEFT;
      }

    for (vsize i = 1; i < infos_.size () - 1; ++i)
      {
        if (flag_directions[i] != CENTER)
          {
            // beamlet count in flag_directions[i] should be preserved
            // which is why we reference the neighbor of opposite direction
            Direction const opposite_dir = -flag_directions[i];
            vsize const neighbor_ind = i + static_cast<vsize> (opposite_dir);
            // if the neighbor has higher beamlet count, then
            // the neighbor should be the one chipping their beamlet count
            if (infos_[i].count () >= infos_[neighbor_ind].count ())
              infos_[i].beam_count_drul_[opposite_dir] -= std::max (
                infos_[i].count () - infos_[neighbor_ind].count (), 1u);
          }
      }
  }

  if (options.subdivide_beams_ && options.maximum_subdivision_interval_.num ()
      && isfinite (options.minimum_subdivision_interval_)
      && options.minimum_subdivision_interval_
           <= options.maximum_subdivision_interval_)
    subdivide_beams (options);

  // stems at boundaries of tuplets should not have beamlets sticking out
  // of the tuplet range
  for (vsize i = 1; i < infos_.size () - 1; ++i)
    {
      if (at_span_start (i))
        infos_[i].beam_count_drul_[LEFT]
          = std::min (beamlet_count (i, LEFT), beamlet_count (i - 1, RIGHT));
      else if (at_span_stop (i))
        infos_[i].beam_count_drul_[RIGHT]
          = std::min (beamlet_count (i, RIGHT), beamlet_count (i + 1, LEFT));
    }
}

/*
  Temporary class that stores current_moment_ and next_moment_ for
  each tuplet span layer. When new tuplets are introduced, they get
  their own Span_position to keep track. When a tuplet ends,
  the position context of the parent tuplet span goes back into effect
*/
class Span_position
{
private:
  Rational const base_moment_;
  unsigned beat_length_; // stays constant
  Rational current_moment_, next_moment_;
  int moment_num_ = -1;

public:
  Rational const end_moment_;
  Tuplet_description const *const tuplet_;

  Span_position (Tuplet_description const &); // for tuplets
  Span_position (Rational const &, unsigned, Rational const &,
                 Rational const &); // instantiated only once, as the root
  void update (Rational const &);
  Rational const &current_moment () const;
  Rational const &next_moment () const;
  Rational next_beat () const;
  unsigned beat_level () const;
};

// Since tuplet start may be negative, current_moment_ must
// be set to same value of next_moment_ to be safe.
// If we have a sextuplet, beat_length_ should be 3
Span_position::Span_position (Tuplet_description const &tuplet)
  : base_moment_ ((tuplet.tuplet_stop () - tuplet.tuplet_start ())
                  / tuplet.denominator_),
    beat_length_ (tuplet.denominator_
                  // last set bit of tuplet.denominator_
                  / (tuplet.denominator_ & -tuplet.denominator_)),
    current_moment_ (tuplet.tuplet_start ()),
    next_moment_ (tuplet.tuplet_start ()),
    end_moment_ (tuplet.tuplet_stop ()),
    tuplet_ (&tuplet)
{
}
Span_position::Span_position (Rational const &base_moment, unsigned beat_length,
                              Rational const &start, Rational const &end)
  : base_moment_ (base_moment),
    beat_length_ (beat_length),
    current_moment_ (start),
    next_moment_ (start),
    end_moment_ (end),
    tuplet_ (nullptr)
{
}

// Must be called before each stem to align the moments
void
Span_position::update (Rational const &pos)
{
  while (next_moment_ <= pos)
    {
      current_moment_ = next_moment_;
      next_moment_ += base_moment_;
      ++moment_num_;
    }
}
Rational const &
Span_position::current_moment () const
{
  return current_moment_;
}
Rational const &
Span_position::next_moment () const
{
  return next_moment_;
}

unsigned
Span_position::beat_level () const
{
  // Incomplete 'beats' or start of tuplet span should not have
  // their rhythmic importance value lowered
  if (!moment_num_ || static_cast<unsigned> (moment_num_) % beat_length_)
    return 0;

  int const beat_num = moment_num_ / static_cast<int> (beat_length_);
  return static_cast<unsigned> (intlog2 (beat_num & -beat_num) + 1);
}

void
Beaming_pattern::set_rhythmic_importance (Beaming_options const &options)
{
  // span_contexts will always be non empty since there always exists
  // the root span
  unsigned const time_signature_numerator
    = scm_is_pair (options.time_signature_)
        ? from_scm<unsigned> (scm_car (options.time_signature_))
        : 1;
  bool const divide_beats
    = !(time_signature_numerator & 1
        || (time_signature_numerator & (time_signature_numerator - 1)) == 0);
  std::forward_list<Span_position> span_contexts {
    {options.measure_length_ / (divide_beats ? time_signature_numerator : 1),
     divide_beats ? time_signature_numerator
                      / (time_signature_numerator & -time_signature_numerator)
                  : 1,
     infos_[0].start_moment_ - measure_offset_,
     end_moment (infos_.size () - 1)}};
  // infos_[i].duration_.factor_ is not sufficient for calculations for certain
  // cases, so we must manually alter the current factor based on incoming and
  // outgoing tuplet spans
  Rational current_factor (1);

  for (vsize i = 0; i < infos_.size (); ++i)
    {
      Rational const &stem_pos = infos_[i].start_moment_;

      // Delete expired tuplet spans
      while (Tuplet_description const *const cur_tuplet
             = span_contexts.front ().tuplet_)
        {
          if (cur_tuplet->tuplet_stop () > stem_pos)
            break;
          // Undo the expired tuplet span factor to the current factor
          (current_factor /= cur_tuplet->numerator_)
            *= cur_tuplet->denominator_;
          span_contexts.pop_front ();
        }

      // Insert tuplet spans that are not already added
      {
        auto insert_position = span_contexts.cbefore_begin ();
        Tuplet_description const *const current_parent
          = span_contexts.front ().tuplet_;
        Tuplet_description const *tuplet_it = infos_[i].tuplet_;
        while (tuplet_it != current_parent)
          {
            if (tuplet_it->tuplet_start () < stem_pos)
              {
                insert_position
                  = span_contexts.emplace_after (insert_position, *tuplet_it);
                (current_factor *= tuplet_it->numerator_)
                  /= tuplet_it->denominator_;
              }
            // Rhythmic importance of start of tuplet span should
            // be set from parent context. If the first stem
            // is part of a tuplet span, it is not necessarily
            // the first note of the tuplet span, whereas
            // a subsequent stem that has a different tuplet span
            // is guaranteed to be the start of said tuplet span
            // and may break the loop as an optimization
            else if (i > 0)
              break;

            tuplet_it = tuplet_it->parent_;
          }
      }

      // the appropriate Span_position for current/next moment
      Span_position &cur_position = span_contexts.front ();
      cur_position.update (stem_pos);

      // Notice that if the current stem introduces new tuplets,
      // those tuplets' factors aren't used until the next stem.
      // rhythmic_importance_ of stems at start of a tuplet span
      // is irrelevant to the tuplet span, but is needed for the parent
      // span as if the stem represents the whole child tuplet
      if (stem_pos == cur_position.current_moment ())
        infos_[i].rhythmic_importance_
          = rhythmic_importance_for_length (
              (cur_position.next_moment () - cur_position.current_moment ())
              / current_factor)
            - static_cast<int> (cur_position.beat_level ());
      else
        {
          Rational moment_relative_to_beat
            = (stem_pos - cur_position.current_moment ()) / current_factor;

          // We must account for numerator of maximum_subdivision_interval
          // which may be greater than 1. Setting it to beamlet count when
          // moment_relative_to_beat numerator isn't divisible basically
          // means don't subdivide here, even though we technically can in
          // the case of the numerator being a power of 2
          if (isfinite (options.maximum_subdivision_interval_)
              && moment_relative_to_beat.num ()
                   % options.maximum_subdivision_interval_.num ())
            infos_[i].rhythmic_importance_
              = infos_[i].duration_.duration_log () - 2;
          else
            {
              infos_[i].rhythmic_importance_
                = rhythmic_importance_for_position (moment_relative_to_beat);

              // We must preserve the tuplet denominator subdivision structure
              // as without this line, a sextuplet of 6 equal-length notes
              // would subdivide between the 2nd and 3rd notes
              infos_[i].rhythmic_importance_ = std::max (
                infos_[i].rhythmic_importance_,
                rhythmic_importance_for_length (
                  (cur_position.next_moment () - stem_pos) / current_factor));

              // Account for the right side of the subdivision having
              // incomplete length as that should make the rhythmic_importance_
              // value higher
              if (options.respect_incomplete_beams_
                  && end_moment (i) < cur_position.end_moment_)
                infos_[i].rhythmic_importance_ = std::max (
                  infos_[i].rhythmic_importance_,
                  rhythmic_importance_for_length (
                    (cur_position.end_moment_ - stem_pos) / current_factor));
            }
        }
    }
}

void
Beaming_pattern::subdivide_beams (Beaming_options const &options)
{
  // if minimum beam subdivision interval is 0, don't bother with it
  bool const check_minimum_subdivision_count
    = isfinite (options.maximum_subdivision_interval_),
    check_maximum_subdivision_count
    = options.minimum_subdivision_interval_.num ();
  // meaning of min/max for beam count is opposite of min/max for interval
  // since we are taking the logarithm of the denominators (basically
  // negating the logarithm of the whole fraction)
  int minimum_subdivision_count = 0, maximum_subdivision_count = 0;

  if (check_minimum_subdivision_count)
    minimum_subdivision_count = rhythmic_importance_for_position (
      options.maximum_subdivision_interval_);

  if (check_maximum_subdivision_count)
    {
      maximum_subdivision_count = rhythmic_importance_for_position (
        options.minimum_subdivision_interval_);

      if (check_minimum_subdivision_count)
        maximum_subdivision_count
          += intlog2 (options.maximum_subdivision_interval_.num ())
             - intlog2 (options.minimum_subdivision_interval_.num ());
    }

  if (!check_minimum_subdivision_count || !check_maximum_subdivision_count
      || minimum_subdivision_count <= maximum_subdivision_count)
    {
      if (!check_minimum_subdivision_count || minimum_subdivision_count < 1)
        minimum_subdivision_count = 1;
      // beam counts will be set to at least
      // minimum_subdivision_beam_count_level, whereas
      // maximum_subdivision_beam_count_level is only used to
      // compare rhythmic importance
      for (vsize i = 1; i < infos_.size () - 1; ++i)
        {
          unsigned const predicted_left_count
            = static_cast<unsigned> (
              std::max (static_cast<int> (infos_[i].rhythmic_importance_),
                        minimum_subdivision_count)),
            predicted_right_count = static_cast<unsigned> (
              std::max (static_cast<int> (infos_[i + 1].rhythmic_importance_),
                        minimum_subdivision_count));

          // we can only chip off one side
          if ((!check_maximum_subdivision_count
               || infos_[i].rhythmic_importance_ <= maximum_subdivision_count)
              && predicted_left_count < beamlet_count (i, LEFT)
              && beamlet_count (i, RIGHT) == infos_[i].count ())
            infos_[i].beam_count_drul_[LEFT] = predicted_left_count;

          else if ((!check_maximum_subdivision_count
                    || infos_[i + 1].rhythmic_importance_
                         <= maximum_subdivision_count)
                   && predicted_right_count < beamlet_count (i, RIGHT)
                   && beamlet_count (i, LEFT) == infos_[i].count ())
            infos_[i].beam_count_drul_[RIGHT] = predicted_right_count;
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
        unsigned const b
          = std::min (infos_[i].count (), infos_[i - 1].count ());
        infos_[i].beam_count_ = b;
        infos_[i].beam_count_drul_[LEFT] = b;
        infos_[i].beam_count_drul_[RIGHT] = b;
      }

  if (!infos_.empty ())
    for (vsize i = 0; i < infos_.size () - 1; i++)
      if (infos_[i].invisible_)
        {
          unsigned const b
            = std::min (infos_[i].count (), infos_[i + 1].count ());
          infos_[i].beam_count_ = b;
          infos_[i].beam_count_drul_[LEFT] = b;
          infos_[i].beam_count_drul_[RIGHT] = b;
        }
}

void
Beaming_pattern::add_stem (Rational const &m, bool inv,
                           Duration const &duration,
                           Tuplet_description const *tuplet)
{
  if (!infos_.empty () && m <= infos_.back ().start_moment_)
    programming_error (
      "stem moment is less than or equal to than previous stem moment");
  infos_.emplace_back (m, inv, duration, tuplet);
}

unsigned
Beaming_pattern::beamlet_count (vsize i, Direction d) const
{
  return infos_.at (i).beam_count_drul_[d];
}

Rational const &
Beaming_pattern::start_moment (vsize i) const
{
  return infos_[i].start_moment_;
}
Rational
Beaming_pattern::end_moment (vsize i) const
{
  return infos_[i].start_moment_ + infos_[i].duration_.get_length ();
}

bool
Beaming_pattern::at_span_start (vsize i) const
{
  return start_moment (i)
         == (infos_[i].tuplet_
               ? std::max (infos_[i].tuplet_->tuplet_start (), start_moment (0))
               : start_moment (0));
}
bool
Beaming_pattern::at_span_stop (vsize i) const
{
  Rational const last_moment = end_moment (infos_.size () - 1);
  return end_moment (i)
         == (infos_[i].tuplet_
               ? std::min (infos_[i].tuplet_->tuplet_stop (), last_moment)
               : last_moment);
}
/*
    Split a beaming pattern at index i and return a new
    Beaming_pattern containing the removed elements
*/
Beaming_pattern *
Beaming_pattern::split_pattern (vsize i, Rational const &measure_length)
{
  std::unique_ptr<Beaming_pattern> new_pattern (new Beaming_pattern (
    (end_moment (i) - (infos_[0].start_moment_ - measure_offset_))
      .mod_rat (measure_length)));

  for (vsize j = i + 1; j < infos_.size (); j++)
    new_pattern->add_stem (infos_[j].start_moment_, infos_[j].invisible_,
                           infos_[j].duration_, infos_[j].tuplet_);

  while (infos_.size () > i + 1)
    infos_.pop_back ();

  return new_pattern.release ();
}

void
Beaming_options::from_context (Context const *context)
{
  subdivide_beams_ = from_scm<bool> (get_property (context, "subdivideBeams"));
  strict_beat_beaming_
    = from_scm<bool> (get_property (context, "strictBeatBeaming"));
  respect_incomplete_beams_
    = from_scm<bool> (get_property (context, "respectIncompleteBeams"));

  beat_structure_ = get_property (context, "beatStructure");
  base_moment_
    = from_scm (get_property (context, "baseMoment"), Moment (1, 4)).main_part_;
  measure_length_
    = from_scm (get_property (context, "measureLength"), Moment (4, 4))
        .main_part_;
  time_signature_ = get_property (context, "timeSignatureFraction");

  minimum_subdivision_interval_
    = from_scm (get_property (context, "minimumBeamSubdivisionInterval"),
                Moment (0))
        .main_part_;
  maximum_subdivision_interval_
    = from_scm (get_property (context, "maximumBeamSubdivisionInterval"),
                Moment::infinity ())
        .main_part_;
}

void
Beaming_options::gc_mark () const
{
  scm_gc_mark (beat_structure_);
  scm_gc_mark (time_signature_);
}
