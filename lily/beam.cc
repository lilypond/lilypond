/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

/*
  TODO:

  - Determine auto knees based on positions if it's set by the user.

  - the code is littered with * and / staff_space calls for
  #'positions. Consider moving to real-world coordinates?

  Problematic issue is user tweaks (user tweaks are in staff-coordinates.)

  Notes:

  - Stems run to the Y-center of the beam.

  - beam_translation is the offset between Y centers of the beam.
*/

#include "beam.hh"

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "beam-scoring-problem.hh"
#include "beaming-pattern.hh"
#include "directional-element-interface.hh"
#include "grob-array.hh"
#include "international.hh"
#include "interval-set.hh"
#include "item.hh"
#include "lookup.hh"
#include "main.hh"
#include "misc.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "rhythmic-head.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "stem.hh"
#include "warn.hh"

#if DEBUG_BEAM_SCORING
#include "font-interface.hh" // debug output.
#include "text-interface.hh" // debug output.
#endif

#include <algorithm>
#include <map>

using std::map;
using std::string;
using std::vector;

// like abs(a - b) but works for both signed and unsigned
// TODO: Move this to some header?
template <class T>
static T
absdiff (T const &a, T const &b)
{
  return std::max (a, b) - std::min (a, b);
}

Beam_stem_segment::Beam_stem_segment ()
{
  max_connect_ = 1000; // infinity
  stem_ = 0;
  width_ = 0.0;
  stem_x_ = 0.0;
  rank_ = 0;
  stem_index_ = 0;
  dir_ = CENTER;
}

bool
beam_segment_less (Beam_segment const &a, Beam_segment const &b)
{
  return a.horizontal_[LEFT] < b.horizontal_[LEFT];
}

Beam_segment::Beam_segment () { vertical_count_ = 0; }

void
Beam::add_stem (Grob *me, Grob *s)
{
  if (Stem::get_beam (s))
    {
      programming_error ("Stem already has beam");
      return;
    }

  Pointer_group_interface::add_grob (me, ly_symbol2scm ("stems"), s);
  s->set_object ("beam", me->self_scm ());
  add_bound_item (dynamic_cast<Spanner *> (me), dynamic_cast<Item *> (s));
}

Real
Beam::get_beam_thickness (Grob *me)
{
  return robust_scm2double (me->get_property ("beam-thickness"), 0)
         * Staff_symbol_referencer::staff_space (me);
}

/* Return the translation between 2 adjoining beams. */
Real
Beam::get_beam_translation (Grob *me)
{
  int beam_count = get_beam_count (me);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real line = Staff_symbol_referencer::line_thickness (me);
  Real beam_thickness = get_beam_thickness (me);
  Real fract = robust_scm2double (me->get_property ("length-fraction"), 1.0);

  /*
    if fract != 1.0, as is the case for grace notes, we want the gap
    to decrease too. To achieve this, we divide the thickness by
    fract */
  return (beam_count < 4
              ? (2 * staff_space * fract + line * fract - beam_thickness) / 2.0
              : (3 * staff_space * fract + line * fract - beam_thickness)
                    / 3.0);
}

/* Maximum beam_count. */
int
Beam::get_beam_count (Grob *me)
{
  int m = 0;

  extract_grob_set (me, "stems", stems);
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *stem = stems[i];
      m = std::max (m, (Stem::beam_multiplicity (stem).length () + 1));
    }
  return m;
}

MAKE_SCHEME_CALLBACK (Beam, calc_normal_stems, 1);
SCM
Beam::calc_normal_stems (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  extract_grob_set (me, "stems", stems);
  SCM val = Grob_array::make_array ();
  Grob_array *ga = unsmob<Grob_array> (val);
  for (vsize i = 0; i < stems.size (); i++)
    if (Stem::is_normal_stem (stems[i]))
      ga->add (stems[i]);

  return val;
}

MAKE_SCHEME_CALLBACK (Beam, calc_direction, 1);
SCM
Beam::calc_direction (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  /* Beams with less than 2 two stems don't make much sense, but could happen
     when you do

     r8[ c8 r8]

  */

  Direction dir = CENTER;

  vsize count = normal_stem_count (me);
  if (count < 2)
    {
      extract_grob_set (me, "stems", stems);
      if (stems.size () == 0)
        {
          me->warning (_ ("removing beam with no stems"));
          me->suicide ();

          return SCM_UNSPECIFIED;
        }
      else
        {
          Grob *stem = first_normal_stem (me);

          /*
            This happens for chord tremolos.
          */
          if (!stem)
            stem = stems[0];

          if (is_direction (stem->get_property_data ("direction")))
            dir = to_dir (stem->get_property_data ("direction"));
          else
            dir = to_dir (stem->get_property ("default-direction"));

          extract_grob_set (stem, "note-heads", heads);
          /* default position of Kievan heads with beams is down
             placing this here avoids warnings downstream */
          if (heads.size ())
            {
              if (scm_is_eq (heads[0]->get_property ("style"),
                             ly_symbol2scm ("kievan")))
                {
                  if (dir == CENTER)
                    dir = DOWN;
                }
            }
        }
    }

  if (count >= 1)
    {
      if (!dir)
        dir = get_default_dir (me);

      consider_auto_knees (me);
    }

  if (dir)
    {
      set_stem_directions (me, dir);
    }

  return scm_from_int (dir);
}

/* We want a maximal number of shared beams, but if there is choice, we
 * take the one that is closest to the end of the stem. This is for
 * situations like
 *
 *        x
 *       |
 *       |
 *   |===|
 *   |=
 *   |
 *  x
 */
int
position_with_maximal_common_beams (SCM left_beaming, SCM right_beaming,
                                    Direction left_dir, Direction right_dir)
{
  Slice lslice = int_list_to_slice (scm_cdr (left_beaming));

  int best_count = 0;
  int best_start = 0;
  for (int i = lslice[-left_dir]; (i - lslice[left_dir]) * left_dir <= 0;
       i += left_dir)
    {
      int count = 0;
      for (SCM s = scm_car (right_beaming); scm_is_pair (s); s = scm_cdr (s))
        {
          int k = -right_dir * scm_to_int (scm_car (s)) + i;
          if (scm_is_true (ly_memv (scm_from_int (k), left_beaming)))
            count++;
        }

      if (count >= best_count)
        {
          best_count = count;
          best_start = i;
        }
    }

  return best_start;
}

MAKE_SCHEME_CALLBACK (Beam, calc_beaming, 1)
SCM
Beam::calc_beaming (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  extract_grob_set (me, "stems", stems);

  Slice last_int;
  last_int.set_empty ();

  SCM last_beaming = scm_cons (SCM_EOL, scm_list_1 (scm_from_int (0)));
  Direction last_dir = CENTER;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *this_stem = stems[i];
      SCM this_beaming = this_stem->get_property ("beaming");

      Direction this_dir = get_grob_direction (this_stem);
      if (scm_is_pair (last_beaming) && scm_is_pair (this_beaming))
        {
          int start_point = position_with_maximal_common_beams (
              last_beaming, this_beaming, last_dir ? last_dir : this_dir,
              this_dir);

          Slice new_slice;
          for (LEFT_and_RIGHT (d))
            {
              new_slice.set_empty ();
              SCM s = index_get_cell (this_beaming, d);
              for (; scm_is_pair (s); s = scm_cdr (s))
                {
                  int new_beam_pos
                      = start_point - this_dir * scm_to_int (scm_car (s));

                  new_slice.add_point (new_beam_pos);
                  scm_set_car_x (s, scm_from_int (new_beam_pos));
                }
            }

          if (!new_slice.is_empty ())
            last_int = new_slice;
        }
      else
        {
          /*
            FIXME: what's this for?
           */
          SCM s = scm_cdr (this_beaming);
          for (; scm_is_pair (s); s = scm_cdr (s))
            {
              int np = -this_dir * scm_to_int (scm_car (s));
              scm_set_car_x (s, scm_from_int (np));
              last_int.add_point (np);
            }
        }

      if (scm_ilength (scm_cdr (this_beaming)) > 0)
        {
          last_beaming = this_beaming;
          last_dir = this_dir;
        }
    }

  return SCM_EOL;
}

bool
operator< (Beam_stem_segment const &a, Beam_stem_segment const &b)
{
  return a.rank_ < b.rank_;
}

typedef map<int, vector<Beam_stem_segment>> Position_stem_segments_map;

MAKE_SCHEME_CALLBACK (Beam, calc_beam_segments, 1);
SCM
Beam::calc_beam_segments (SCM smob)
{
  /* ugh, this has a side-effect that we need to ensure that
     Stem.beaming is correct */
  Grob *me_grob = unsmob<Grob> (smob);
  (void)me_grob->get_property ("beaming");

  Spanner *me = dynamic_cast<Spanner *> (me_grob);

  extract_grob_set (me, "stems", stems);

  Grob *commonx = common_refpoint_of_array (stems, me, X_AXIS);
  for (LEFT_and_RIGHT (d))
    commonx = me->get_bound (d)->common_refpoint (commonx, X_AXIS);

  int gap_count = robust_scm2int (me->get_property ("gap-count"), 0);
  Real gap_length = robust_scm2double (me->get_property ("gap"), 0.0);

  Position_stem_segments_map stem_segments;
  Real lt = Staff_symbol_referencer::line_thickness (me);

  /* There are two concepts of "rank" that are used in the following code.
     The beam_rank is the vertical position of the beam (larger numbers are
     closer to the noteheads). Beam_stem_segment.rank_, on the other hand,
     is the horizontal position of the segment (this is incremented by two
     for each stem; the beam segment on the right side of the stem has
     a higher rank (by one) than its neighbour to the left). */
  Slice ranks;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *stem = stems[i];
      Real stem_width
          = robust_scm2double (stem->get_property ("thickness"), 1.0) * lt;
      Real stem_x = stem->relative_coordinate (commonx, X_AXIS);
      SCM beaming = stem->get_property ("beaming");

      for (LEFT_and_RIGHT (d))
        {
          // Find the maximum and minimum beam ranks.
          // Given that RANKS is never reset to empty, the interval will always
          // be smallest for the left beamlet of the first stem, and then it
          // might grow. Do we really want this? (It only affects the tremolo
          // gaps) --jneem
          for (SCM s = index_get_cell (beaming, d); scm_is_pair (s);
               s = scm_cdr (s))
            {
              if (!scm_is_integer (scm_car (s)))
                continue;

              int beam_rank = scm_to_int (scm_car (s));
              ranks.add_point (beam_rank);
            }

          for (SCM s = index_get_cell (beaming, d); scm_is_pair (s);
               s = scm_cdr (s))
            {
              if (!scm_is_integer (scm_car (s)))
                continue;

              int beam_rank = scm_to_int (scm_car (s));
              Beam_stem_segment seg;
              seg.stem_ = stem;
              seg.stem_x_ = stem_x;
              seg.rank_ = 2 * i + (d + 1) / 2;
              seg.width_ = stem_width;
              seg.stem_index_ = i;
              seg.dir_ = d;
              seg.max_connect_ = robust_scm2int (
                  stem->get_property ("max-beam-connect"), 1000);

              Direction stem_dir = get_grob_direction (stem);

              seg.gapped_ = (stem_dir * beam_rank
                             < (stem_dir * ranks[-stem_dir] + gap_count));
              stem_segments[beam_rank].push_back (seg);
            }
        }
    }

  Drul_array<Real> break_overshoot = robust_scm2drul (
      me->get_property ("break-overshoot"), Drul_array<Real> (-0.5, 0.0));

  vector<Beam_segment> segments;
  for (Position_stem_segments_map::const_iterator i (stem_segments.begin ());
       i != stem_segments.end (); i++)
    {
      vector<Beam_stem_segment> segs = (*i).second;
      vector_sort (segs, std::less<Beam_stem_segment> ());

      Beam_segment current;

      // Iterate over all of the segments of the current beam rank,
      // merging the adjacent Beam_stem_segments into one Beam_segment
      // when appropriate.
      int vertical_count = (*i).first;
      for (vsize j = 0; j < segs.size (); j++)
        {
          // Keeping track of the different directions here is a little tricky.
          // segs[j].dir_ is the direction of the beam segment relative to the
          // stem (ie. segs[j].dir_ == LEFT if the beam segment sticks out to
          // the left of its stem) whereas event_dir refers to the edge of the
          // beam segment that we are currently looking at (ie. if segs[j].dir_
          // == event_dir then we are looking at that edge of the beam segment
          // that is furthest from its stem).
          Beam_stem_segment const &seg = segs[j];
          for (LEFT_and_RIGHT (event_dir))
            {
              // TODO: make names clearer? --jneem
              // on_line_bound: whether the current segment is on the boundary
              // of the WHOLE beam on_beam_bound: whether the current segment is
              // on the boundary of just that part
              //   of the beam with the current beam_rank
              bool on_line_bound = (seg.dir_ == LEFT)
                                       ? seg.stem_index_ == 0
                                       : seg.stem_index_ == stems.size () - 1;
              bool on_beam_bound
                  = (event_dir == LEFT) ? j == 0 : j == segs.size () - 1;
              bool inside_stem = (event_dir == LEFT)
                                     ? seg.stem_index_ > 0
                                     : seg.stem_index_ + 1 < stems.size ();

              bool event = on_beam_bound
                           || absdiff (seg.rank_, segs[j + event_dir].rank_) > 1
                           || (abs (vertical_count) >= seg.max_connect_
                               || abs (vertical_count)
                                      >= segs[j + event_dir].max_connect_);

              if (!event)
                // Then this edge of the current segment is irrelevant because
                // it will be connected with the next segment in the event_dir
                // direction. If we skip the left edge here, the right edge of
                // the previous segment has already been skipped since
                // the conditions are symmetric
                continue;

              current.vertical_count_ = vertical_count;
              current.horizontal_[event_dir] = seg.stem_x_;
              if (seg.dir_ == event_dir)
                // then we are examining the edge of a beam segment that is
                // furthest from its stem.
                {
                  if (on_line_bound
                      && me->get_bound (event_dir)->break_status_dir ())
                    {
                      current.horizontal_[event_dir]
                          = (Axis_group_interface::generic_bound_extent (
                                 me->get_bound (event_dir), commonx,
                                 X_AXIS)[RIGHT]
                             + event_dir * break_overshoot[event_dir]);
                    }
                  else
                    {
                      Grob *stem = stems[seg.stem_index_];
                      Drul_array<Real> beamlet_length = robust_scm2interval (
                          stem->get_property ("beamlet-default-length"),
                          Interval (1.1, 1.1));
                      Drul_array<Real> max_proportion = robust_scm2interval (
                          stem->get_property ("beamlet-max-length-proportion"),
                          Interval (0.75, 0.75));
                      Real length = beamlet_length[seg.dir_];

                      if (inside_stem)
                        {
                          Grob *neighbor_stem
                              = stems[seg.stem_index_ + event_dir];
                          Real neighbor_stem_x
                              = neighbor_stem->relative_coordinate (commonx,
                                                                    X_AXIS);

                          length = std::min (
                              length, fabs (neighbor_stem_x - seg.stem_x_)
                                          * max_proportion[seg.dir_]);
                        }
                      current.horizontal_[event_dir] += event_dir * length;
                    }
                }
              else
                // we are examining the edge of a beam segment that is closest
                // (ie. touching, unless there is a gap) its stem.
                {
                  current.horizontal_[event_dir] += event_dir * seg.width_ / 2;
                  if (seg.gapped_)
                    {
                      current.horizontal_[event_dir] -= event_dir * gap_length;

                      if (Stem::is_invisible (seg.stem_))
                        {
                          /*
                            Need to do this in case of whole notes. We don't
                            want the heads to collide with the beams.
                           */
                          extract_grob_set (seg.stem_, "note-heads", heads);

                          for (vsize k = 0; k < heads.size (); k++)
                            current.horizontal_[event_dir]
                                = event_dir
                                  * std::min (
                                      event_dir
                                          * current.horizontal_[event_dir],
                                      -gap_length / 2
                                          + event_dir
                                                * heads[k]->extent (
                                                    commonx,
                                                    X_AXIS)[-event_dir]);
                        }
                    }
                }

              if (event_dir == RIGHT)
                {
                  segments.push_back (current);
                  current = Beam_segment ();
                }
            }
        }
    }

  SCM segments_scm = SCM_EOL;

  for (vsize i = segments.size (); i--;)
    {
      segments_scm = scm_cons (
          scm_list_2 (scm_cons (ly_symbol2scm ("vertical-count"),
                                scm_from_int (segments[i].vertical_count_)),
                      scm_cons (ly_symbol2scm ("horizontal"),
                                ly_interval2scm (segments[i].horizontal_))),
          segments_scm);
    }

  return segments_scm;
}

MAKE_SCHEME_CALLBACK (Beam, calc_x_positions, 1);
SCM
Beam::calc_x_positions (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  SCM segments = me->get_property ("beam-segments");
  Interval x_positions;
  x_positions.set_empty ();
  for (SCM s = segments; scm_is_pair (s); s = scm_cdr (s))
    x_positions.unite (robust_scm2interval (
        ly_assoc_get (ly_symbol2scm ("horizontal"), scm_car (s), SCM_EOL),
        Interval (0.0, 0.0)));

  // Case for beams without segments (i.e. uniting two skips with a beam)
  // TODO: should issue a warning?  warning likely issued downstream, but
  // couldn't hurt...
  if (x_positions.is_empty ())
    {
      extract_grob_set (me, "stems", stems);
      Grob *common_x = common_refpoint_of_array (stems, me, X_AXIS);
      for (LEFT_and_RIGHT (d))
        x_positions[d] = me->relative_coordinate (common_x, X_AXIS);
    }
  return ly_interval2scm (x_positions);
}

vector<Beam_segment>
Beam::get_beam_segments (Grob *me)
{
  SCM segments_scm = me->get_property ("beam-segments");
  vector<Beam_segment> segments;
  for (SCM s = segments_scm; scm_is_pair (s); s = scm_cdr (s))
    {
      segments.push_back (Beam_segment ());
      segments.back ().vertical_count_ = robust_scm2int (
          ly_assoc_get (ly_symbol2scm ("vertical-count"), scm_car (s), SCM_EOL),
          0);
      segments.back ().horizontal_ = robust_scm2interval (
          ly_assoc_get (ly_symbol2scm ("horizontal"), scm_car (s), SCM_EOL),
          Interval (0.0, 0.0));
    }

  return segments;
}

MAKE_SCHEME_CALLBACK (Beam, print, 1);
SCM
Beam::print (SCM grob)
{
  Spanner *me = unsmob<Spanner> (grob);
  /*
    TODO - mild code dup for all the commonx calls.
    Some use just common_refpoint_of_array, some (in print and
    calc_beam_segments) use this plus calls to get_bound.

    Figure out if there is any particular reason for this and
    consolidate in one Beam::get_common function.
  */
  extract_grob_set (me, "stems", stems);
  Grob *commonx = common_refpoint_of_array (stems, me, X_AXIS);
  for (LEFT_and_RIGHT (d))
    commonx = me->get_bound (d)->common_refpoint (commonx, X_AXIS);

  vector<Beam_segment> segments = get_beam_segments (me);

  if (!segments.size ())
    return SCM_EOL;

  Real blot = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

  SCM posns = me->get_property ("quantized-positions");
  Interval span
      = robust_scm2interval (me->get_property ("X-positions"), Interval (0, 0));
  Interval pos;
  if (!is_number_pair (posns))
    {
      programming_error ("no beam positions?");
      pos = Interval (0, 0);
    }
  else
    pos = ly_scm2realdrul (posns);

  scale_drul (&pos, Staff_symbol_referencer::staff_space (me));

  Real dy = pos[RIGHT] - pos[LEFT];
  Real slope = (dy && span.length ()) ? dy / span.length () : 0;

  Real beam_thickness = get_beam_thickness (me);
  Real beam_dy = get_beam_translation (me);

  Direction feather_dir = to_dir (me->get_property ("grow-direction"));

  Interval placements = robust_scm2interval (
      me->get_property ("normalized-endpoints"), Interval (0.0, 0.0));

  Stencil the_beam;
  vsize extreme
      = (segments[0].vertical_count_ == 0 ? segments[0].vertical_count_
                                          : segments.back ().vertical_count_);

  for (vsize i = 0; i < segments.size (); i++)
    {
      Real local_slope = slope;
      /*
        Makes local slope proportional to the ratio of the length of this beam
        to the total length.
      */
      if (feather_dir)
        local_slope += (feather_dir * segments[i].vertical_count_ * beam_dy
                        * placements.length () / span.length ());

      Stencil b = Lookup::beam (local_slope, segments[i].horizontal_.length (),
                                beam_thickness, blot);

      b.translate_axis (segments[i].horizontal_[LEFT], X_AXIS);
      Real multiplier = feather_dir ? placements[LEFT] : 1.0;

      Interval weights (1 - multiplier, multiplier);

      if (feather_dir != LEFT)
        weights.swap ();

      // we need two translations: the normal one and
      // the one of the lowest segment
      size_t idx[] = {i, extreme};
      Real translations[2];

      for (int j = 0; j < 2; j++)
        translations[j] = slope
                              * (segments[idx[j]].horizontal_[LEFT]
                                 - span.linear_combination (CENTER))
                          + pos.linear_combination (CENTER)
                          + beam_dy * segments[idx[j]].vertical_count_;

      Real weighted_average
          = translations[0] * weights[LEFT] + translations[1] * weights[RIGHT];

      /*
        Tricky.  The manipulation of the variable `weighted_average' below
        ensures that beams with a RIGHT grow direction will start from the
        position of the lowest segment at 0, and this error will decrease and
        decrease over the course of the beam.  Something with a LEFT grow
        direction, on the other hand, will always start in the correct place but
        progressively accrue error at broken places.  This code shifts beams up
        given where they are in the total span length (controlled by the
        variable `multiplier').  To better understand what it does, try
        commenting it out: you'll see that all of the RIGHT growing beams
        immediately start too low and get better over line breaks, whereas all
        of the LEFT growing beams start just right and get worse over line
        breaks.
      */
      Real factor = Interval (multiplier, 1 - multiplier)
                        .linear_combination (feather_dir);

      if (segments[0].vertical_count_ < 0 && feather_dir)
        {
          Real n = static_cast<Real> (segments.size () - 1);
          weighted_average += beam_dy * n * factor;
        }

      b.translate_axis (weighted_average, Y_AXIS);

      the_beam.add_stencil (b);
    }

#if (DEBUG_BEAM_SCORING)
  SCM annotation = me->get_property ("annotation");
  if (scm_is_string (annotation))
    {
      extract_grob_set (me, "stems", stems);

      /*
        This code prints the demerits for each beam. Perhaps this
        should be switchable for those who want to twiddle with the
        parameters.
      */
      string str;
      SCM properties = Font_interface::text_font_alist_chain (me);

      properties = scm_cons (
          scm_acons (ly_symbol2scm ("font-size"), scm_from_int (-5), SCM_EOL),
          properties);

      Direction stem_dir
          = stems.size () ? to_dir (stems[0]->get_property ("direction")) : UP;

      Stencil score = *unsmob<Stencil> (Text_interface::interpret_markup (
          me->layout ()->self_scm (), properties, annotation));

      if (!score.is_empty ())
        {
          score.translate_axis (me->relative_coordinate (commonx, X_AXIS),
                                X_AXIS);
          the_beam.add_at_edge (Y_AXIS, stem_dir, score, 1.0);
        }
    }
#endif

  the_beam.translate_axis (-me->relative_coordinate (commonx, X_AXIS), X_AXIS);
  return the_beam.smobbed_copy ();
}

Direction
Beam::get_default_dir (Grob *me)
{
  extract_grob_set (me, "stems", stems);

  Drul_array<Real> extremes (0.0, 0.0);
  for (vector<Grob *>::const_iterator s = stems.begin (); s != stems.end ();
       s++)
    {
      Interval positions = Stem::head_positions (*s);
      for (DOWN_and_UP (d))
        {
          if (sign (positions[d]) == d)
            extremes[d] = d * std::max (d * positions[d], d * extremes[d]);
        }
    }

  Drul_array<int> total (0, 0);
  Drul_array<int> count (0, 0);

  bool force_dir = false;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];
      Direction stem_dir = CENTER;
      SCM stem_dir_scm = s->get_property_data ("direction");
      if (is_direction (stem_dir_scm))
        {
          stem_dir = to_dir (stem_dir_scm);
          force_dir = true;
        }
      else
        stem_dir = to_dir (s->get_property ("default-direction"));

      if (!stem_dir)
        stem_dir = to_dir (s->get_property ("neutral-direction"));

      if (stem_dir)
        {
          count[stem_dir]++;
          total[stem_dir] += std::max (
              int (-stem_dir * Stem::head_positions (s)[-stem_dir]), 0);
        }
    }

  if (!force_dir)
    {
      if (abs (extremes[UP]) > -extremes[DOWN])
        return DOWN;
      else if (extremes[UP] < -extremes[DOWN])
        return UP;
    }

  Direction dir = CENTER;
  Direction d = CENTER;
  if ((d = (Direction)sign (count[UP] - count[DOWN])))
    dir = d;
  else if (count[UP] && count[DOWN]
           && (d = (Direction)sign (total[UP] / count[UP]
                                    - total[DOWN] / count[DOWN])))
    dir = d;
  else if ((d = (Direction)sign (total[UP] - total[DOWN])))
    dir = d;
  else
    dir = to_dir (me->get_property ("neutral-direction"));

  return dir;
}

/* Set all stems with non-forced direction to beam direction.
   Urg: non-forced should become `without/with unforced' direction,
   once stem gets cleaned-up. */
void
Beam::set_stem_directions (Grob *me, Direction d)
{
  extract_grob_set (me, "stems", stems);

  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

      SCM forcedir = s->get_property_data ("direction");
      if (!to_dir (forcedir))
        set_grob_direction (s, d);
    }
}

/*
  Only try horizontal beams for knees.  No reliable detection of
  anything else is possible here, since we don't know funky-beaming
  settings, or X-distances (slopes!)  People that want sloped
  knee-beams, should set the directions manually.


  TODO:

  this routine should take into account the stemlength scoring
  of a possible knee/nonknee beam.
*/
void
Beam::consider_auto_knees (Grob *me)
{
  SCM scm = me->get_property ("auto-knee-gap");
  if (!scm_is_number (scm))
    return;

  vector<Interval> forbidden_intervals;

  extract_grob_set (me, "normal-stems", stems);

  Grob *common = common_refpoint_of_array (stems, me, Y_AXIS);
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  vector<Interval> head_extents_array;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *stem = stems[i];

      Interval head_extents;
      if (Stem::head_count (stem))
        {
          head_extents = Stem::head_positions (stem);
          head_extents.widen (1);
          head_extents *= staff_space * 0.5;

          /*
            We could subtract beam Y position, but this routine only
            sets stem directions, a constant shift does not have an
            influence.
          */
          head_extents += stem->pure_relative_y_coordinate (common, 0, INT_MAX);

          if (to_dir (stem->get_property_data ("direction")))
            {
              Direction stemdir = to_dir (stem->get_property ("direction"));
              head_extents[-stemdir] = -stemdir * infinity_f;
            }
        }
      head_extents_array.push_back (head_extents);

      forbidden_intervals.push_back (head_extents);
    }

  Interval max_gap;
  Real max_gap_len = 0.0;

  vector<Interval> allowed_regions
      = Interval_set::interval_union (forbidden_intervals)
            .complement ()
            .intervals ();
  for (vsize i = allowed_regions.size () - 1; i != VPOS; i--)
    {
      Interval gap = allowed_regions[i];

      /*
        the outer gaps are not knees.
      */
      if (std::isinf (gap[LEFT]) || std::isinf (gap[RIGHT]))
        continue;

      if (gap.length () >= max_gap_len)
        {
          max_gap_len = gap.length ();
          max_gap = gap;
        }
    }

  Real beam_translation = get_beam_translation (me);
  Real beam_thickness = Beam::get_beam_thickness (me);
  int beam_count = Beam::get_beam_count (me);
  Real height_of_beams
      = beam_thickness / 2 + (beam_count - 1) * beam_translation;
  Real threshold = scm_to_double (scm) + height_of_beams;

  if (max_gap_len > threshold)
    {
      int j = 0;
      for (vsize i = 0; i < stems.size (); i++)
        {
          Grob *stem = stems[i];
          Interval head_extents = head_extents_array[j++];

          Direction d
              = (head_extents.center () < max_gap.center ()) ? UP : DOWN;

          stem->set_property ("direction", scm_from_int (d));

          head_extents.intersect (max_gap);
          assert (head_extents.is_empty () || head_extents.length () < 1e-6);
        }
    }
}

MAKE_SCHEME_CALLBACK (Beam, calc_stem_shorten, 1)
SCM
Beam::calc_stem_shorten (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  /*
    shortening looks silly for x staff beams
  */
  if (is_knee (me))
    return scm_from_int (0);

  Real forced_fraction = static_cast<Real> (forced_stem_count (me))
                         / static_cast<Real> (normal_stem_count (me));

  int beam_count = get_beam_count (me);

  SCM shorten_list = me->get_property ("beamed-stem-shorten");
  if (scm_is_null (shorten_list))
    return scm_from_int (0);

  Real staff_space = Staff_symbol_referencer::staff_space (me);

  SCM shorten_elt = robust_list_ref (beam_count - 1, shorten_list);
  Real shorten = scm_to_double (shorten_elt) * staff_space;

  shorten *= forced_fraction;

  if (shorten)
    return scm_from_double (shorten);

  return scm_from_double (0.0);
}

MAKE_SCHEME_CALLBACK (Beam, quanting, 3);
SCM
Beam::quanting (SCM smob, SCM ys_scm, SCM align_broken_intos)
{
  Grob *me = unsmob<Grob> (smob);
  Drul_array<Real> ys
      = robust_scm2drul (ys_scm, Drul_array<Real> (infinity_f, -infinity_f));
  bool cbs = to_boolean (align_broken_intos);

  Beam_scoring_problem problem (me, ys, cbs);
  ys = problem.solve ();

  return ly_interval2scm (ys);
}

/*
  Report slice containing the numbers that are both in (car BEAMING)
  and (cdr BEAMING)
*/
Slice
where_are_the_whole_beams (SCM beaming)
{
  Slice l;

  for (SCM s = scm_car (beaming); scm_is_pair (s); s = scm_cdr (s))
    {
      if (scm_is_true (ly_memv (scm_car (s), scm_cdr (beaming))))

        l.add_point (scm_to_int (scm_car (s)));
    }

  return l;
}

/* Return the Y position of the stem-end, given the Y-left, Y-right
   in POS for stem S.  This Y position is relative to S. */
Real
Beam::calc_stem_y (Grob *me, Grob *stem, Grob **common, Real xl, Real xr,
                   Direction feather_dir, Drul_array<Real> pos, bool french)
{
  Real beam_translation = get_beam_translation (me);
  Direction stem_dir = get_grob_direction (stem);

  Real dx = xr - xl;
  Real relx
      = dx ? (stem->relative_coordinate (common[X_AXIS], X_AXIS) - xl) / dx : 0;
  Real xdir = 2 * relx - 1;

  Real stem_y = linear_combination (pos, xdir);

  SCM beaming = stem->get_property ("beaming");

  Slice beam_slice (french ? where_are_the_whole_beams (beaming)
                           : Stem::beam_multiplicity (stem));
  if (beam_slice.is_empty ())
    beam_slice = Slice (0, 0);
  Interval beam_multiplicity (beam_slice[LEFT], beam_slice[RIGHT]);

  /*
    feather dir = 1 , relx 0->1 : factor 0 -> 1
    feather dir = 0 , relx 0->1 : factor 1 -> 1
    feather dir = -1, relx 0->1 : factor 1 -> 0
   */
  Real feather_factor = 1;
  if (feather_dir > 0)
    feather_factor = relx;
  else if (feather_dir < 0)
    feather_factor = 1 - relx;

  stem_y += feather_factor * beam_translation
            * beam_multiplicity[Direction (((french) ? DOWN : UP) * stem_dir)];
  Real id = me->relative_coordinate (common[Y_AXIS], Y_AXIS)
            - stem->relative_coordinate (common[Y_AXIS], Y_AXIS);

  return stem_y + id;
}

/*
  Hmm.  At this time, beam position and slope are determined.  Maybe,
  stem directions and length should set to relative to the chord's
  position of the beam.  */
MAKE_SCHEME_CALLBACK (Beam, set_stem_lengths, 1);
SCM
Beam::set_stem_lengths (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  /* trigger callbacks. */
  (void)me->get_property ("direction");
  (void)me->get_property ("beaming");

  SCM posns = me->get_property ("positions");

  extract_grob_set (me, "stems", stems);
  if (!stems.size ())
    return posns;

  Grob *common[2];
  for (int a = 2; a--;)
    common[a] = common_refpoint_of_array (stems, me, Axis (a));

  Drul_array<Real> pos = ly_scm2realdrul (posns);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  scale_drul (&pos, staff_space);

  bool gap = false;
  Real thick = 0.0;
  if (robust_scm2int (me->get_property ("gap-count"), 0))
    {
      gap = true;
      thick = get_beam_thickness (me);
    }

  Grob *fvs = first_normal_stem (me);
  Grob *lvs = last_normal_stem (me);

  Interval x_span
      = robust_scm2interval (me->get_property ("X-positions"), Interval (0, 0));
  Direction feather_dir = to_dir (me->get_property ("grow-direction"));

  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

      bool french = to_boolean (s->get_property ("french-beaming"));
      Real stem_y
          = calc_stem_y (me, s, common, x_span[LEFT], x_span[RIGHT],
                         feather_dir, pos, french && s != lvs && s != fvs);

      /*
        Make the stems go up to the end of the beam. This doesn't matter
        for normal beams, but for tremolo beams it looks silly otherwise.
      */
      if (gap && !Stem::is_invisible (s))
        stem_y += thick * 0.5 * get_grob_direction (s);

      /*
        Do set_stem_positions for invisible stems too, so tuplet brackets
        have a reference point for sloping
       */
      Stem::set_stem_positions (s, 2 * stem_y / staff_space);
    }

  return posns;
}

void
Beam::set_beaming (Grob *me, Beaming_pattern const *beaming)
{
  extract_grob_set (me, "stems", stems);

  for (vsize i = 0; i < stems.size (); i++)
    {
      /*
        Don't overwrite user settings.
      */
      for (LEFT_and_RIGHT (d))
        {
          Grob *stem = stems[i];
          SCM beaming_prop = stem->get_property ("beaming");
          if (scm_is_null (beaming_prop)
              || scm_is_null (index_get_cell (beaming_prop, d)))
            {
              int count = beaming->beamlet_count (i, d);
              if (i > 0 && i + 1 < stems.size () && Stem::is_invisible (stem))
                count = std::min (count, beaming->beamlet_count (i, -d));

              if (((i == 0 && d == LEFT)
                   || (i == stems.size () - 1 && d == RIGHT))
                  && stems.size () > 1
                  && to_boolean (me->get_property ("clip-edges")))
                count = 0;

              Stem::set_beaming (stem, count, d);
            }
        }
    }
}

vsize
Beam::forced_stem_count (Grob *me)
{
  extract_grob_set (me, "normal-stems", stems);

  vsize f = 0;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

      /* I can imagine counting those boundaries as a half forced stem,
         but let's count them full for now. */
      Direction defdir = to_dir (s->get_property ("default-direction"));

      if (abs (Stem::chord_start_y (s)) > 0.1 && defdir
          && get_grob_direction (s) != defdir)
        f++;
    }
  return f;
}

vsize
Beam::normal_stem_count (Grob *me)
{
  extract_grob_set (me, "normal-stems", stems);
  return stems.size ();
}

Grob *
Beam::first_normal_stem (Grob *me)
{
  extract_grob_set (me, "normal-stems", stems);
  return stems.size () ? stems[0] : 0;
}

Grob *
Beam::last_normal_stem (Grob *me)
{
  extract_grob_set (me, "normal-stems", stems);
  return stems.size () ? stems.back () : 0;
}

/*
  [TODO]

  handle rest under beam (do_post: beams are calculated now)
  what about combination of collisions and rest under beam.

  Should lookup

  rest -> stem -> beam -> interpolate_y_position ()
*/
MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Beam, rest_collision_callback, 2, 1, "");
SCM
Beam::rest_collision_callback (SCM smob, SCM prev_offset)
{
  if (!scm_is_number (prev_offset))
    prev_offset = SCM_INUM0;

  Grob *rest = unsmob<Grob> (smob);
  if (scm_is_number (rest->get_property ("staff-position")))
    return prev_offset;

  Grob *stem = unsmob<Grob> (rest->get_object ("stem"));

  if (!stem)
    return prev_offset;

  Grob *beam = unsmob<Grob> (stem->get_object ("beam"));
  if (!beam || !has_interface<Beam> (beam) || !Beam::normal_stem_count (beam))
    return prev_offset;

  Grob *common_y = rest->common_refpoint (beam, Y_AXIS);

  Drul_array<Real> pos (robust_scm2drul (beam->get_property ("positions"),
                                         Drul_array<Real> (0, 0)));

  for (LEFT_and_RIGHT (dir))
    pos[dir] += beam->relative_coordinate (common_y, Y_AXIS);

  Real staff_space = Staff_symbol_referencer::staff_space (rest);

  scale_drul (&pos, staff_space);

  Real dy = pos[RIGHT] - pos[LEFT];

  extract_grob_set (beam, "stems", stems);
  Grob *common = common_refpoint_of_array (stems, beam, X_AXIS);

  Interval x_span = robust_scm2interval (beam->get_property ("X-positions"),
                                         Interval (0.0, 0.0));
  Real x0 = x_span[LEFT];
  Real dx = x_span.length ();
  Real slope = dy && dx ? dy / dx : 0;

  Direction d = get_grob_direction (stem);
  Real stem_y
      = pos[LEFT] + (stem->relative_coordinate (common, X_AXIS) - x0) * slope;

  Real beam_translation = get_beam_translation (beam);
  Real beam_thickness = Beam::get_beam_thickness (beam);

  /*
    TODO: this is not strictly correct for 16th knee beams.
  */
  int beam_count = Stem::beam_multiplicity (stem).length () + 1;

  Real height_of_my_beams
      = beam_thickness / 2 + (beam_count - 1) * beam_translation;
  Real beam_y = stem_y - d * height_of_my_beams;

  Real offset = robust_scm2double (prev_offset, 0.0);
  Interval rest_extent = rest->extent (rest, Y_AXIS);
  rest_extent.translate (offset + rest->parent_relative (common_y, Y_AXIS));

  Real rest_dim = rest_extent[d];
  Real minimum_distance
      = staff_space
        * (robust_scm2double (stem->get_property ("stemlet-length"), 0.0)
           + robust_scm2double (rest->get_property ("minimum-distance"), 0.0));

  Real shift
      = d * std::min (d * (beam_y - d * minimum_distance - rest_dim), 0.0);

  shift /= staff_space;

  /* Always move discretely by half spaces */
  shift = ceil (fabs (shift * 2.0)) / 2.0 * sign (shift);

  Interval staff_span = Staff_symbol_referencer::staff_span (rest);
  staff_span *= staff_space / 2;

  /* Inside staff, move by whole spaces*/
  if (staff_span.contains (rest_extent[d] + staff_space * shift)
      || staff_span.contains (rest_extent[-d] + staff_space * shift))
    shift = ceil (fabs (shift)) * sign (shift);

  return scm_from_double (offset + staff_space * shift);
}

/*
  Estimate the position of a rest under a beam,
  using the average position of its neighboring heads.
*/
MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Beam, pure_rest_collision_callback, 4, 1,
                                   "");
SCM
Beam::pure_rest_collision_callback (SCM smob, SCM, /* start */
                                    SCM,           /* end */
                                    SCM prev_offset)
{
  if (!scm_is_number (prev_offset))
    prev_offset = SCM_INUM0;

  Grob *me = unsmob<Grob> (smob);
  Grob *stem = unsmob<Grob> (me->get_object ("stem"));
  if (!stem)
    return prev_offset;
  Grob *beam = unsmob<Grob> (stem->get_object ("beam"));
  if (!beam || !Beam::normal_stem_count (beam)
      || !is_direction (beam->get_property_data ("direction")))
    return prev_offset;

  Real ss = Staff_symbol_referencer::staff_space (me);

  extract_grob_set (beam, "stems", stems);
  vector<Grob *> my_stems;

  for (vsize i = 0; i < stems.size (); i++)
    if (Stem::head_count (stems[i]) || stems[i] == stem)
      my_stems.push_back (stems[i]);

  vsize idx = -1;

  for (vsize i = 0; i < my_stems.size (); i++)
    if (my_stems[i] == stem)
      {
        idx = i;
        break;
      }
  Grob *left;
  Grob *right;

  if (idx == (vsize)-1 || my_stems.size () == 1)
    return prev_offset;
  else if (idx == 0)
    left = right = my_stems[1];
  else if (idx == my_stems.size () - 1)
    left = right = my_stems[idx - 1];
  else
    {
      left = my_stems[idx - 1];
      right = my_stems[idx + 1];
    }

  /* Estimate the closest beam to be four positions away from the heads, */
  Direction beamdir = get_grob_direction (beam);
  Real beam_pos = (Stem::head_positions (left)[beamdir]
                   + Stem::head_positions (right)[beamdir])
                      / 2.0
                  + 4.0 * beamdir; // four staff-positions
  /* and that the closest beam never crosses staff center by more than two
   * positions */
  beam_pos = std::max (-2.0, beam_pos * beamdir) * beamdir;

  Real minimum_distance
      = ss
        * (robust_scm2double (stem->get_property ("stemlet-length"), 0.0)
           + robust_scm2double (me->get_property ("minimum-distance"), 0.0));
  Real offset = beam_pos * ss / 2.0 - minimum_distance * beamdir
                - me->extent (me, Y_AXIS)[beamdir];
  Real previous = robust_scm2double (prev_offset, 0.0);

  /* Always move by a whole number of staff spaces, always away from the beam */
  offset = floor (std::min (0.0, (offset - previous) / ss * beamdir)) * ss
               * beamdir
           + previous;

  return scm_from_double (offset);
}

bool
Beam::is_knee (Grob *me)
{
  SCM k = me->get_property ("knee");
  if (scm_is_bool (k))
    return ly_scm2bool (k);

  bool knee = false;
  int d = 0;
  extract_grob_set (me, "stems", stems);
  for (vsize i = stems.size (); i--;)
    {
      Direction dir = get_grob_direction (stems[i]);
      if (d && d != dir)
        {
          knee = true;
          break;
        }
      d = dir;
    }

  me->set_property ("knee", ly_bool2scm (knee));

  return knee;
}

bool
Beam::is_cross_staff (Grob *me)
{
  extract_grob_set (me, "stems", stems);
  Grob *staff_symbol = Staff_symbol_referencer::get_staff_symbol (me);
  for (vsize i = 0; i < stems.size (); i++)
    if (Staff_symbol_referencer::get_staff_symbol (stems[i]) != staff_symbol)
      return true;
  return false;
}

MAKE_SCHEME_CALLBACK (Beam, calc_cross_staff, 1)
SCM
Beam::calc_cross_staff (SCM smob)
{
  return scm_from_bool (is_cross_staff (unsmob<Grob> (smob)));
}

int
Beam::get_direction_beam_count (Grob *me, Direction d)
{
  extract_grob_set (me, "stems", stems);
  int bc = 0;

  for (vsize i = stems.size (); i--;)
    {
      /*
        Should we take invisible stems into account?
      */
      if (get_grob_direction (stems[i]) == d)
        bc = std::max (bc, (Stem::beam_multiplicity (stems[i]).length () + 1));
    }

  return bc;
}

ADD_INTERFACE (Beam,
               "A beam.\n"
               "\n"
               "The @code{beam-thickness} property is the weight of beams,"
               " measured in staffspace.  The @code{direction} property is"
               " not user-serviceable.  Use the @code{direction} property"
               " of @code{Stem} instead.\n"
               "The following properties may be set in the @code{details}"
               " list.\n"
               "\n"
               "@table @code\n"
               "@item stem-length-demerit-factor\n"
               "Demerit factor used for inappropriate stem lengths.\n"
               "@item secondary-beam-demerit\n"
               "Demerit used in quanting calculations for multiple"
               " beams.\n"
               "@item region-size\n"
               "Size of region for checking quant scores.\n"
               "@item beam-eps\n"
               "Epsilon for beam quant code to check for presence"
               " in gap.\n"
               "@item stem-length-limit-penalty\n"
               "Penalty for differences in stem lengths on a beam.\n"
               "@item damping-direction-penalty\n"
               "Demerit penalty applied when beam direction is different"
               " from damping direction.\n"
               "@item hint-direction-penalty\n"
               "Demerit penalty applied when beam direction is different"
               " from damping direction, but damping slope is"
               " <= @code{round-to-zero-slope}.\n"
               "@item musical-direction-factor\n"
               "Demerit scaling factor for difference between"
               " beam slope and music slope.\n"
               "@item ideal-slope-factor\n"
               "Demerit scaling factor for difference between"
               " beam slope and damping slope.\n"
               "@item round-to-zero-slope\n"
               "Damping slope which is considered zero for purposes of"
               " calculating direction penalties.\n"
               "@end table\n",

               /* properties */
               "annotation "
               "auto-knee-gap "
               "beamed-stem-shorten "
               "beaming "
               "beam-segments "
               "beam-thickness "
               "break-overshoot "
               "clip-edges "
               "concaveness "
               "collision-interfaces "
               "collision-voice-only "
               "covered-grobs "
               "damping "
               "details "
               "direction "
               "gap "
               "gap-count "
               "grow-direction "
               "inspect-quants "
               "knee "
               "length-fraction "
               "least-squares-dy "
               "neutral-direction "
               "normal-stems "
               "positions "
               "quantized-positions "
               "shorten "
               "skip-quanting "
               "stems "
               "X-positions ");
