/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

  TODO: This is way too hairy

  TODO: fix naming.

  Stem-end, chord-start, etc. is all confusing naming.

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
  Note that several internal functions have a calc_beam bool argument.
  This argument means: "If set, acknowledge the fact that there is a beam
  and deal with it.  If not, give me the measurements as if there is no beam."
  Most pure functions are called WITHOUT calc_beam, whereas non-pure functions
  are called WITH calc_beam.

  The only exception to this is ::pure_height, which calls internal_pure_height
  with "true" for calc_beam in order to trigger the calculations of other
  pure heights in case there is a beam.  It passes false, however, to
  internal_height and internal_pure_height for all subsequent iterations.
*/

#include "stem.hh"
#include "spanner.hh"

#include "beam.hh"
#include "directional-element-interface.hh"
#include "dot-column.hh"
#include "font-interface.hh"
#include "international.hh"
#include "lookup.hh"
#include "misc.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "rest.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stem-tremolo.hh"
#include "warn.hh"

#include <algorithm>
#include <cmath> // rint
#include <vector>

using std::string;
using std::vector;

void
Stem::set_beaming (Grob *me, int beam_count, Direction d)
{
  SCM pair = get_property (me, "beaming");

  if (!scm_is_pair (pair))
    {
      pair = scm_cons (SCM_EOL, SCM_EOL);
      set_property (me, "beaming", pair);
    }

  SCM lst = index_get_cell (pair, d);
  if (beam_count)
    for (int i = 0; i < beam_count; i++)
      lst = scm_cons (to_scm (i), lst);
  else
    lst = SCM_BOOL_F;

  index_set_cell (pair, d, lst);
}

int
Stem::get_beaming (Grob *me, Direction d)
{
  SCM pair = get_property (me, "beaming");
  if (!scm_is_pair (pair))
    return 0;

  SCM lst = index_get_cell (pair, d);
  if (scm_is_false (lst))
    return 0;

  // This list represents the vertical positions at which beams start/end at
  // this stem, so the O(n) cost of scm_length is fine.
  return from_scm<int> (scm_length (lst));
}

Interval
Stem::head_positions (Grob *me)
{
  if (head_count (me))
    {
      Drul_array<Grob *> e (extremal_heads (me));
      return Interval (Staff_symbol_referencer::get_position (e[DOWN]),
                       Staff_symbol_referencer::get_position (e[UP]));
    }
  return Interval ();
}

Real
Stem::chord_start_y (Grob *me)
{
  if (head_count (me))
    return Staff_symbol_referencer::get_position (last_head (me))
           * Staff_symbol_referencer::staff_space (me) * 0.5;

  return 0;
}

void
Stem::set_stem_positions (Grob *me, Real se, Real fc)
{
  // todo: margins
  Direction d = get_grob_direction (me);

  Grob *beam = unsmob<Grob> (get_object (me, "beam"));
  if (d && d * head_positions (me)[get_grob_direction (me)] >= se * d)
    me->warning (_ ("weird stem size, check for narrow beams"));

  // trigger note collision mechanisms
  Real stem_beg = internal_calc_stem_begin_position (me, false);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real half_space = staff_space * 0.5;

  Interval height;
  height[-d] = stem_beg * half_space;
  height[d] = se * half_space + beam_end_corrective (me);

  Real stemlet_length
    = from_scm<double> (get_property (me, "stemlet-length"), 0.0);
  bool stemlet = stemlet_length > 0.0;

  Grob *lh = get_reference_head (me);

  if (!lh)
    {
      if (stemlet && beam)
        {
          Real beam_translation = Beam::get_beam_translation (beam);
          Real beam_thickness = Beam::get_beam_thickness (beam);
          int beam_count = beam_multiplicity (me).length () + 1;

          height[-d]
            = (height[d]
               - d
                   * (0.5 * beam_thickness
                      + beam_translation * std::max (0, (beam_count - 1))
                      + stemlet_length));
        }
      else if (!stemlet && beam)
        height[-d] = height[d];
      else if (stemlet && !beam)
        me->programming_error ("Can't have a stemlet without a beam.");
    }

  set_property (me, "stem-begin-position",
                to_scm (height[-d] * 2 / staff_space));
  set_property (me, "length", to_scm (height.length () * 2 / staff_space));

  if (fc)
    set_property (me, "french-beaming-stem-adjustment", to_scm (fc));
}

/* Note head that determines hshift for upstems
   WARNING: triggers direction  */
Grob *
Stem::support_head (Grob *me)
{
  extract_grob_set (me, "note-heads", heads);
  if (heads.size () == 1)
    return heads[0];

  return first_head (me);
}

vsize
Stem::head_count (Grob *me)
{
  return Pointer_group_interface::count (me, ly_symbol2scm ("note-heads"));
}

/* The note head which forms one end of the stem.
   WARNING: triggers direction  */
Grob *
Stem::first_head (Grob *me)
{
  Direction d = get_grob_direction (me);
  if (d)
    return extremal_heads (me)[-d];
  return 0;
}

/* The note head opposite to the first head.  */
Grob *
Stem::last_head (Grob *me)
{
  Direction d = get_grob_direction (me);
  if (d)
    return extremal_heads (me)[d];
  return 0;
}

/*
  START is part where stem reaches `last' head.

  This function returns a drul with (bottom-head, top-head).
*/
Drul_array<Grob *>
Stem::extremal_heads (Grob *me)
{
  // N.B. `me` could be a NoteColumn rather than a Stem.  This isn't very clean,
  // but this was implemented here first, and rearranging it without rearranging
  // a bunch of other things might do more harm than good. [DE]

  const int inf = INT_MAX;
  Drul_array<int> extpos;
  extpos[DOWN] = inf;
  extpos[UP] = -inf;

  Drul_array<Grob *> exthead;
  extract_grob_set (me, "note-heads", heads);

  for (vsize i = 0; i < heads.size (); i++)
    {
      Grob *n = heads[i];
      int p = Staff_symbol_referencer::get_rounded_position (n);

      if (p < extpos[DOWN]) /* < lowest note unison: take FIRST one */
        {
          exthead[DOWN] = n;
          extpos[DOWN] = p;
        }
      if (p >= extpos[UP]) /* >= highest note unison: take LAST one */
        {
          exthead[UP] = n;
          extpos[UP] = p;
        }
    }

  return exthead;
}

/* The staff positions, in ascending order.
 * If FILTER, include the main column of noteheads only */
vector<int>
Stem::note_head_positions (Grob *me, bool filter)
{
  vector<int> ps;
  extract_grob_set (me, "note-heads", heads);
  Grob *xref = common_refpoint_of_array (heads, me, X_AXIS);

  for (vsize i = heads.size (); i--;)
    {
      Grob *n = heads[i];
      if (filter && n->relative_coordinate (xref, X_AXIS) != 0.0)
        continue;

      int p = Staff_symbol_referencer::get_rounded_position (n);
      ps.push_back (p);
    }

  std::sort (ps.begin (), ps.end ());
  return ps;
}

void
Stem::add_head (Grob *me, Grob *n)
{
  set_object (n, "stem", me->self_scm ());

  if (has_interface<Note_head> (n))
    Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-heads"), n);
  else if (has_interface<Rest> (n))
    Pointer_group_interface::add_grob (me, ly_symbol2scm ("rests"), n);
}

bool
Stem::is_invisible (Grob *me)
{
  if (is_normal_stem (me))
    return false;
  else if (head_count (me))
    return true;
  else // if there are no note-heads, we might want stemlets
    return 0.0 == from_scm<double> (get_property (me, "stemlet-length"), 0.0);
}

bool
Stem::is_normal_stem (Grob *me)
{
  if (!head_count (me))
    return false;

  return from_scm<int> (get_property (me, "duration-log")) >= 1;
}

MAKE_SCHEME_CALLBACK (Stem, pure_height, "ly:stem::pure-height", 3)
SCM
Stem::pure_height (SCM smob, SCM /* start */, SCM /* end */)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (internal_pure_height (me, true));
}

Interval
Stem::internal_pure_height (Grob *me, bool calc_beam)
{
  if (!is_normal_stem (me))
    return Interval (0.0, 0.0);

  Grob *beam = unsmob<Grob> (get_object (me, "beam"));

  Interval iv = internal_height (me, false);

  if (!beam)
    return iv;
  if (calc_beam)
    {
      Interval overshoot;
      Direction dir = get_grob_direction (me);
      for (const auto d : {DOWN, UP})
        overshoot[d] = d == dir ? dir * infinity_f : iv[d];

      vector<Interval> heights;
      vector<Grob *> my_stems;
      extract_grob_set (beam, "normal-stems", normal_stems);
      for (vsize i = 0; i < normal_stems.size (); i++)
        if (get_grob_direction (normal_stems[i]) == dir)
          {
            if (normal_stems[i] != me)
              heights.push_back (
                Stem::internal_pure_height (normal_stems[i], false));
            else
              heights.push_back (iv);
            my_stems.push_back (normal_stems[i]);
          }
      //iv.unite (heights.back ());
      // look for cross staff effects
      vector<Real> coords;
      Grob *common = common_refpoint_of_array (my_stems, me, Y_AXIS);
      Real min_pos = infinity_f;
      Real max_pos = -infinity_f;
      for (vsize i = 0; i < my_stems.size (); i++)
        {
          coords.push_back (
            my_stems[i]->pure_relative_y_coordinate (common, 0, INT_MAX));
          min_pos = std::min (min_pos, coords[i]);
          max_pos = std::max (max_pos, coords[i]);
        }
      for (vsize i = 0; i < heights.size (); i++)
        {
          heights[i][dir]
            += dir == DOWN ? coords[i] - max_pos : coords[i] - min_pos;
        }

      for (vsize i = 0; i < heights.size (); i++)
        iv.unite (heights[i]);

      for (vsize i = 0; i < my_stems.size (); i++)
        cache_pure_height (my_stems[i], iv, heights[i]);
      iv.intersect (overshoot);
    }

  return iv;
}

void
Stem::cache_pure_height (Grob *me, Interval iv, Interval my_iv)
{
  Interval overshoot;
  Direction dir = get_grob_direction (me);
  for (const auto d : {DOWN, UP})
    overshoot[d] = d == dir ? dir * infinity_f : my_iv[d];

  iv.intersect (overshoot);
  dynamic_cast<Item *> (me)->cache_pure_height (iv);
}

MAKE_SCHEME_CALLBACK (Stem, calc_stem_end_position,
                      "ly:stem::calc-stem-end-position", 1)
SCM
Stem::calc_stem_end_position (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (internal_calc_stem_end_position (me, true));
}

MAKE_SCHEME_CALLBACK (Stem, pure_calc_stem_end_position,
                      "ly:stem::pure-calc-stem-end-position", 3)
SCM
Stem::pure_calc_stem_end_position (SCM smob, SCM, /* start */
                                   SCM /* end */)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (internal_calc_stem_end_position (me, false));
}

Real
Stem::internal_calc_stem_end_position (Grob *me, bool calc_beam)
{
  if (!head_count (me))
    return 0.0;

  Grob *beam = get_beam (me);
  Real ss = Staff_symbol_referencer::staff_space (me);
  Direction dir = get_grob_direction (me);

  if (beam && calc_beam)
    {
      (void) get_property (beam, "quantized-positions");
      return from_scm<double> (get_property (me, "length"), 0.0)
             + dir
                 * from_scm<double> (get_property (me, "stem-begin-position"),
                                     0.0);
    }

  vector<Real> a;

  /* WARNING: IN HALF SPACES */
  SCM details = get_property (me, "details");
  int durlog = duration_log (me);

  Real staff_rad = Staff_symbol_referencer::staff_radius (me);
  Real length = 7;
  SCM s = ly_assoc_get (ly_symbol2scm ("lengths"), details, SCM_EOL);
  if (scm_is_pair (s))
    length = 2 * from_scm<double> (robust_list_ref (durlog - 2, s));

  /* Stems in unnatural (forced) direction should be shortened,
     according to [Roush & Gourlay] */
  Interval hp = head_positions (me);
  if (dir && dir * hp[dir] >= 0)
    {
      SCM sshorten
        = ly_assoc_get (ly_symbol2scm ("stem-shorten"), details, SCM_EOL);
      SCM scm_shorten
        = scm_is_pair (sshorten)
            ? robust_list_ref (std::max (duration_log (me) - 2, 0), sshorten)
            : SCM_EOL;
      Real shorten_property = 2 * from_scm<double> (scm_shorten, 0);
      /*  change in length between full-size and shortened stems is executed gradually.
          "transition area" = stems between full-sized and fully-shortened.
          */
      Real quarter_stem_length = 2 * from_scm<double> (robust_list_ref (0, s));
      /*  shortening_step = difference in length between consecutive stem lengths
          in transition area. The bigger the difference between full-sized
          and shortened stems, the bigger shortening_step is.
          (but not greater than 1/2 and not smaller than 1/4).
          value 6 is heuristic; it determines the suggested transition slope steepnesas.
          */
      Real shortening_step
        = std::min (std::max (0.25, (shorten_property / 6)), 0.5);
      /*  Shortening of unflagged stems should begin on the first stem that sticks
          more than 1 staffspace (2 units) out of the staff.
          Shortening of flagged stems begins in the same moment as unflagged ones,
          but not earlier than on the middle line note.
          */
      Real which_step
        = (std::min (1.0, quarter_stem_length - (2 * staff_rad) - 2.0))
          + abs (hp[dir]);
      Real shorten = std::min (std::max (0.0, (shortening_step * which_step)),
                               shorten_property);

      length -= shorten;
    }

  length *= from_scm<double> (get_property (me, "length-fraction"), 1.0);

  /* Tremolo stuff.  */
  Grob *t_flag = unsmob<Grob> (get_object (me, "tremolo-flag"));
  if (t_flag && (!unsmob<Grob> (get_object (me, "beam")) || !calc_beam))
    {
      /* Crude hack: add extra space if tremolo flag is there.

      We can't do this for the beam, since we get into a loop
      (Stem_tremolo::raw_stencil () looks at the beam.) --hwn  */

      Real minlen = 1.0 + 2 * Stem_tremolo::vertical_length (t_flag) / ss;

      /* We don't want to add the whole extent of the flag because the trem
         and the flag can overlap partly. beam_translation gives a good
         approximation */
      if (durlog >= 3)
        {
          Real beam_trans = Stem_tremolo::get_beam_translation (t_flag);
          /* the obvious choice is (durlog - 2) here, but we need a bit more space. */
          minlen += 2 * (durlog - 1.5) * beam_trans;

          /* up-stems need even a little more space to avoid collisions. This
             needs to be in sync with the tremolo positioning code in
             Stem_tremolo::print */
          if (dir == UP)
            minlen += beam_trans;
        }
      length = std::max (length, minlen + 1.0);
    }

  Real stem_end = dir ? hp[dir] + dir * length : 0;

  /* TODO: change name  to extend-stems to staff/center/'()  */
  bool no_extend = from_scm<bool> (get_property (me, "no-stem-extend"));
  if (!no_extend && dir * stem_end < 0)
    stem_end = 0.0;

  return stem_end;
}

/* The log of the duration (Number of hooks on the flag minus two)  */
int
Stem::duration_log (Grob *me)
{
  SCM s = get_property (me, "duration-log");
  return (scm_is_number (s)) ? from_scm<int> (s) : 2;
}

MAKE_SCHEME_CALLBACK (Stem, calc_positioning_done,
                      "ly:stem::calc-positioning-done", 1);
SCM
Stem::calc_positioning_done (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  if (!head_count (me))
    return SCM_BOOL_T;

  set_property (me, "positioning-done", SCM_BOOL_T);

  extract_grob_set (me, "note-heads", ro_heads);
  vector<Grob *> heads (ro_heads);
  std::sort (heads.begin (), heads.end (), position_less);
  Direction dir = get_strict_grob_direction (me);

  if (dir < CENTER)
    std::reverse (heads.begin (), heads.end ());

  Real thick = thickness (me);

  Grob *hed = support_head (me);

  bool is_harmonic_centered = false;
  for (vsize i = 0; i < heads.size (); i++)
    is_harmonic_centered = is_harmonic_centered
                           || scm_is_eq (get_property (heads[i], "style"),
                                         ly_symbol2scm ("harmonic"));
  is_harmonic_centered = is_harmonic_centered && is_invisible (me);

  Real w = hed->extent (hed, X_AXIS)[dir];
  for (vsize i = 0; i < heads.size (); i++)
    {
      Real amount = w - heads[i]->extent (heads[i], X_AXIS)[dir];

      if (is_harmonic_centered)
        amount = hed->extent (hed, X_AXIS).center ()
                 - heads[i]->extent (heads[i], X_AXIS).center ();

      if (!std::isnan (amount)) // empty heads can produce NaN
        heads[i]->translate_axis (amount, X_AXIS);
    }
  bool parity = true;
  Real lastpos = Staff_symbol_referencer::get_position (heads[0]);
  int threshold = from_scm (get_property (me, "note-collision-threshold"), 1);
  for (vsize i = 1; i < heads.size (); i++)
    {
      Real p = Staff_symbol_referencer::get_position (heads[i]);
      Real dy = fabs (lastpos - p);

      /*
        dy should always be 0.5, 0.0, 1.0, but provide safety margin
        for rounding errors.
      */
      if (dy < 0.1 + threshold)
        {
          if (parity)
            {
              // Don't include the glyph's 'breapth' value.
              Real ell = heads[i]->extent (heads[i], X_AXIS).right ();

              Direction d = get_grob_direction (me);
              /*
                Reversed heads (i.e., heads on the other side of the
                stem) should be shifted by `ell - thickness`, but this
                looks too crowded, so we only shift by `ell -
                0.5*thickness`.

                This leads to an asymmetry: Normal heads overlap the
                stem by 100%, whereas reversed heads only overlap by
                50%.
              */
              Real reverse_overlap = 0.5;

              /*
                However, the first reverse head has to be shifted even
                less if it has the same vertical position as the first
                head, or there will be a gap because of the head slant
                (issue 346).
              */

              if (i == 1 && dy < 0.1)
                reverse_overlap = 1.1;

              if (is_invisible (me))
                {
                  if (duration_log (me) >= 0)
                    {
                      /*
                        Semibreves are positioned considerably nearer
                        to be recognizable as part of the chord rather
                        than being a parallel voice.  During the
                        course of issue 346 there was a discussion to
                        change this for unisons (i.e., dy < 0.1) to
                        reduce overlap but without reaching agreement,
                        and with Gould being rather on the overlapping
                        front.
                      */
                      reverse_overlap = 2;
                    }
                  else
                    {
                      /*
                        Breves and longer are offset 'exactly' so that
                        the vertical lines to the left and right of
                        the note heads align.  This is guaranteed by
                        the glyphs themselves: the left vertical
                        line(s) are in the 'breapth' area, touching
                        the horizontal origin.
                      */
                      reverse_overlap = 0;
                    }
                }

              heads[i]->translate_axis ((ell - thick * reverse_overlap) * d,
                                        X_AXIS);

              /* TODO:

              For some cases we should kern some more: when the
              distance between the next or prev note is too large, we'd
              get large white gaps, eg.

              |
              X|
              |X  <- kern this.
              |
              X

              */
            }
          parity = !parity;
        }
      else
        parity = true;

      lastpos = p;
    }

  return SCM_BOOL_T;
}

MAKE_SCHEME_CALLBACK (Stem, calc_direction, "ly:stem::calc-direction", 1);
SCM
Stem::calc_direction (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Direction dir = CENTER;
  if (Grob *beam = unsmob<Grob> (get_object (me, "beam")))
    {
      SCM ignore_me = get_property (beam, "direction");
      (void) ignore_me;
      dir = get_grob_direction (me);
    }
  else
    {
      SCM dd = get_property (me, "default-direction");
      dir = from_scm<Direction> (dd);
      if (!dir)
        return get_property (me, "neutral-direction");
    }

  return to_scm (dir);
}

MAKE_SCHEME_CALLBACK (Stem, calc_default_direction,
                      "ly:stem::calc-default-direction", 1);
SCM
Stem::calc_default_direction (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  Direction dir = CENTER;
  if (head_count (me))
    {
      int staff_center = 0;
      Interval hp = head_positions (me);
      int udistance = static_cast<int> (UP * hp[UP] - staff_center);
      int ddistance = static_cast<int> (DOWN * hp[DOWN] - staff_center);

      dir = Direction (sign (ddistance - udistance));
    }

  return to_scm (dir);
}

// note - height property necessary to trigger quantized beam positions
// otherwise, we could just use Grob::stencil_height_proc
MAKE_SCHEME_CALLBACK (Stem, height, "ly:stem::height", 1);
SCM
Stem::height (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (internal_height (me, true));
}

Grob *
Stem::get_reference_head (Grob *me)
{
  return from_scm<bool> (get_property (me, "avoid-note-head"))
           ? last_head (me)
           : first_head (me);
}

Real
Stem::beam_end_corrective (Grob *me)
{
  Grob *beam = unsmob<Grob> (get_object (me, "beam"));
  Direction dir = get_grob_direction (me);
  if (beam)
    {
      if (dir == CENTER)
        {
          programming_error ("no stem direction");
          dir = UP;
        }
      return dir * Beam::get_beam_thickness (beam) * 0.5;
    }
  return 0.0;
}

Interval
Stem::internal_height (Grob *me, bool calc_beam)
{
  Grob *beam = get_beam (me);
  if (!is_valid_stem (me) && !beam)
    return Interval ();

  Direction dir = get_grob_direction (me);

  if (beam && calc_beam)
    {
      /* trigger set-stem-lengths. */
      (void) get_property (beam, "quantized-positions");
    }

  /*
    If there is a beam but no stem, slope calculations depend on this
    routine to return where the stem end /would/ be.
  */
  if (calc_beam && !beam
      && !unsmob<const Stencil> (get_property (me, "stencil")))
    return Interval ();

  Real y1 = from_scm<double> (
    (calc_beam ? get_property (me, "stem-begin-position")
               : get_pure_property (me, "stem-begin-position", 0, INT_MAX)),
    0.0);

  Real y2 = dir
              * from_scm<double> (
                (calc_beam ? get_property (me, "length")
                           : get_pure_property (me, "length", 0, INT_MAX)),
                0.0)
            + y1;

  Real half_space = Staff_symbol_referencer::staff_space (me) * 0.5;

  Interval stem_y
    = Interval (std::min (y1, y2), std::max (y2, y1)) * half_space;

  return stem_y;
}

MAKE_SCHEME_CALLBACK (Stem, width, "ly:stem::width", 1);
SCM
Stem::width (SCM e)
{
  auto *const me = LY_ASSERT_SMOB (Grob, e, 1);

  Interval r;

  if (is_invisible (me))
    r.set_empty ();
  else
    {
      r = Interval (-1, 1);
      r *= thickness (me) / 2;
    }

  return to_scm (r);
}

Real
Stem::thickness (Grob *me)
{
  return from_scm<double> (get_property (me, "thickness"))
         * Staff_symbol_referencer::line_thickness (me);
}

MAKE_SCHEME_CALLBACK (Stem, calc_stem_begin_position,
                      "ly:stem::calc-stem-begin-position", 1);
SCM
Stem::calc_stem_begin_position (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (internal_calc_stem_begin_position (me, true));
}

MAKE_SCHEME_CALLBACK (Stem, pure_calc_stem_begin_position,
                      "ly:stem::pure-calc-stem-begin-position", 3);
SCM
Stem::pure_calc_stem_begin_position (SCM smob, SCM, /* start */
                                     SCM /* end */)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (internal_calc_stem_begin_position (me, false));
}

Real
Stem::internal_calc_stem_begin_position (Grob *me, bool calc_beam)
{
  Grob *beam = get_beam (me);
  Real ss = Staff_symbol_referencer::staff_space (me);
  if (beam && calc_beam)
    {
      (void) get_property (beam, "quantized-positions");
      return from_scm<double> (get_property (me, "stem-begin-position"), 0.0);
    }

  Grob *lh = get_reference_head (me);

  if (!lh)
    return 0.0;

  Real pos = Staff_symbol_referencer::get_position (lh);

  if (Grob *head = support_head (me))
    {
      Interval head_height = head->extent (head, Y_AXIS);
      Real y_attach = Note_head::stem_attachment_coordinate (head, Y_AXIS);

      y_attach = head_height.linear_combination (y_attach);
      if (std::isfinite (y_attach)) // empty heads
        pos += y_attach * 2 / ss;
    }

  return pos;
}

MAKE_SCHEME_CALLBACK (Stem, pure_calc_length, "ly:stem::pure-calc-length", 3);
SCM
Stem::pure_calc_length (SCM smob, SCM /*start*/, SCM /*end*/)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Real beg = from_scm<double> (
    get_pure_property (me, "stem-begin-position", 0, INT_MAX), 0.0);
  Real res = fabs (internal_calc_stem_end_position (me, false) - beg);
  return to_scm (res);
}

MAKE_SCHEME_CALLBACK (Stem, calc_length, "ly:stem::calc-length", 1);
SCM
Stem::calc_length (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  if (unsmob<Grob> (get_object (me, "beam")))
    {
      me->programming_error (
        "ly:stem::calc-length called but will not be used for beamed stem.");
      return to_scm (0.0);
    }

  Real beg = from_scm<double> (get_property (me, "stem-begin-position"), 0.0);
  Real res = fabs (internal_calc_stem_end_position (me, true) - beg);
  return to_scm (res);
}

bool
Stem::is_valid_stem (Grob *me)
{
  /* TODO: make the stem start a direction ?
     This is required to avoid stems passing in tablature chords.  */
  if (!me)
    return false;
  Grob *lh = get_reference_head (me);
  Grob *beam = unsmob<Grob> (get_object (me, "beam"));

  if (!lh && !beam)
    return false;

  if (is_invisible (me))
    return false;

  return true;
}

MAKE_SCHEME_CALLBACK (Stem, print, "ly:stem::print", 1);
SCM
Stem::print (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  if (!is_valid_stem (me))
    return SCM_EOL;

  Direction dir = get_grob_direction (me);
  Real y1 = from_scm<double> (get_property (me, "stem-begin-position"), 0.0);
  Real stem_length = from_scm<double> (get_property (me, "length"), 0.0);
  Real fb_stem_adjustment = from_scm<double> (
    get_property (me, "french-beaming-stem-adjustment"), 0.0);
  Real half_space = Staff_symbol_referencer::staff_space (me) * 0.5;

  /* Shorten inner French Beams (for printing) */
  stem_length -= fb_stem_adjustment;

  Real y2 = dir * stem_length + y1;

  Interval stem_y
    = Interval (std::min (y1, y2), std::max (y2, y1)) * half_space;

  stem_y[dir] -= beam_end_corrective (me);

  // URG
  Real stem_width = thickness (me);
  Real blot = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

  Box b = Box (Interval (-stem_width / 2, stem_width / 2), stem_y);

  Stencil mol;
  Stencil ss = Lookup::round_filled_box (b, blot);
  mol.add_stencil (ss);

  return mol.smobbed_copy ();
}

/*
  move the stem to right of the notehead if it is up.
*/
MAKE_SCHEME_CALLBACK (Stem, offset_callback, "ly:stem::offset-callback", 1);
SCM
Stem::offset_callback (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  extract_grob_set (me, "rests", rests);
  if (rests.size ())
    {
      Grob *rest = rests.back ();
      Real r = robust_relative_extent (rest, rest, X_AXIS).center ();
      return to_scm (r);
    }

  if (Grob *f = first_head (me))
    {
      Interval head_wid = f->extent (f, X_AXIS);
      Real attach = 0.0;

      if (is_invisible (me))
        attach = 0.0;
      else
        attach = Note_head::stem_attachment_coordinate (f, X_AXIS);

      Real real_attach = head_wid.linear_combination (attach);
      Real r = std::isnan (real_attach) ? 0.0 : real_attach;

      /* If not centered: correct for stem thickness.  */
      string style
        = robust_symbol2string (get_property (f, "style"), "default");
      if (attach && style != "mensural" && style != "neomensural"
          && style != "petrucci")
        {
          Direction d = get_grob_direction (me);
          Real rule_thick = thickness (me);
          r += -d * rule_thick * 0.5;
        }
      return to_scm (r);
    }

  programming_error ("Weird stem.");
  return to_scm (0.0);
}

Spanner *
Stem::get_beam (Grob *me)
{
  SCM b = get_object (me, "beam");
  return unsmob<Spanner> (b);
}

Stem_info
Stem::get_stem_info (Grob *me)
{
  Stem_info si;
  si.dir_ = get_grob_direction (me);

  SCM scm_info = get_property (me, "stem-info");
  si.ideal_y_ = from_scm<double> (scm_car (scm_info));
  si.shortest_y_ = from_scm<double> (scm_cadr (scm_info));
  return si;
}

MAKE_SCHEME_CALLBACK (Stem, calc_stem_info, "ly:stem::calc-stem-info", 1);
SCM
Stem::calc_stem_info (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Direction my_dir = get_grob_direction (me);

  if (!my_dir)
    {
      programming_error ("no stem dir set");
      my_dir = UP;
    }

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Grob *beam = get_beam (me);

  if (beam)
    {
      (void) get_property (beam, "beaming");
    }

  Real beam_translation = Beam::get_beam_translation (beam);
  Real beam_thickness = Beam::get_beam_thickness (beam);
  int beam_count = Beam::get_direction_beam_count (beam, my_dir);
  Real length_fraction
    = from_scm<double> (get_property (me, "length-fraction"), 1.0);

  /* Simple standard stem length */
  SCM details = get_property (me, "details");
  SCM lengths
    = ly_assoc_get (ly_symbol2scm ("beamed-lengths"), details, SCM_EOL);

  Real ideal_length
    = (scm_is_pair (lengths)
         ? (from_scm<double> (robust_list_ref (beam_count - 1, lengths))
              * staff_space * length_fraction
            /*
            stem only extends to center of beam
          */
            - 0.5 * beam_thickness)
         : 0.0);

  /* Condition: sane minimum free stem length (chord to beams) */
  lengths = ly_assoc_get (ly_symbol2scm ("beamed-minimum-free-lengths"),
                          details, SCM_EOL);

  Real ideal_minimum_free
    = (scm_is_pair (lengths)
         ? (from_scm<double> (robust_list_ref (beam_count - 1, lengths))
            * staff_space * length_fraction)
         : 0.0);

  Real height_of_my_trem = 0.0;
  Grob *trem = unsmob<Grob> (get_object (me, "tremolo-flag"));
  if (trem)
    {
      height_of_my_trem = Stem_tremolo::vertical_length (trem)
                          /* hack a bit of space around the trem. */
                          + beam_translation;
    }

  /* UGH
     It seems that also for ideal minimum length, we must use
     the maximum beam count (for this direction):

     \score { \relative c'' { a8[ a32] } }

     must be horizontal. */
  Real height_of_my_beams
    = beam_thickness + (beam_count - 1) * beam_translation;

  Real ideal_minimum_length = ideal_minimum_free + height_of_my_beams
                              + height_of_my_trem
                              /* stem only extends to center of beam */
                              - 0.5 * beam_thickness;

  ideal_length = std::max (ideal_length, ideal_minimum_length);

  /* Convert to Y position, calculate for dir == UP */
  Real note_start = /* staff positions */
    head_positions (me)[my_dir] * 0.5 * my_dir * staff_space;
  Real ideal_y = note_start + ideal_length;

  /* Conditions for Y position */

  /* Lowest beam of (UP) beam must never be lower than second staffline

  Reference?

  Although this (additional) rule is probably correct,
  I expect that highest beam (UP) should also never be lower
  than middle staffline, just as normal stems.

  Reference?

  Obviously not for grace beams.

  Also, not for knees.  Seems to be a good thing. */
  bool no_extend = from_scm<bool> (get_property (me, "no-stem-extend"));
  bool is_knee = Beam::is_knee (beam);
  if (!no_extend && !is_knee)
    {
      /* Highest beam of (UP) beam must never be lower than middle
         staffline */
      ideal_y = std::max (ideal_y, 0.0);
      /* Lowest beam of (UP) beam must never be lower than second staffline */
      ideal_y = std::max (ideal_y,
                          (-staff_space - beam_thickness + height_of_my_beams));
    }

  ideal_y -= from_scm<double> (get_property (beam, "shorten"), 0);

  SCM bemfl = ly_assoc_get (
    ly_symbol2scm ("beamed-extreme-minimum-free-lengths"), details, SCM_EOL);

  Real minimum_free
    = (scm_is_pair (bemfl)
         ? (from_scm<double> (robust_list_ref (beam_count - 1, bemfl))
            * staff_space * length_fraction)
         : 0.0);

  Real minimum_length = std::max (minimum_free, height_of_my_trem)
                        + height_of_my_beams
                        /* stem only extends to center of beam */
                        - 0.5 * beam_thickness;

  ideal_y *= my_dir;
  Real minimum_y = note_start + minimum_length;
  Real shortest_y = minimum_y * my_dir;

  return ly_list (to_scm (ideal_y), to_scm (shortest_y));
}

Slice
Stem::beam_multiplicity (Grob *stem)
{
  SCM beaming = get_property (stem, "beaming");
  Slice le = int_list_to_slice (scm_car (beaming));
  Slice ri = int_list_to_slice (scm_cdr (beaming));
  le.unite (ri);
  return le;
}

bool
Stem::is_cross_staff (Grob *stem)
{
  Grob *beam = unsmob<Grob> (get_object (stem, "beam"));
  return beam && Beam::is_cross_staff (beam);
}

MAKE_SCHEME_CALLBACK (Stem, calc_cross_staff, "ly:stem::calc-cross-staff", 1)
SCM
Stem::calc_cross_staff (SCM smob)
{
  return to_scm (is_cross_staff (unsmob<Grob> (smob)));
}

Grob *
Stem::flag (Grob *me)
{
  return unsmob<Grob> (get_object (me, "flag"));
}

/* FIXME:  Too many properties  */
ADD_INTERFACE (Stem,
               R"(
The stem represents the graphical stem.  In addition, it internally connects
note heads, beams, and tremolos.  Rests and whole notes have invisible stems.

The following properties may be set in the @code{details} list.

@table @code
@item beamed-lengths
List of stem lengths given beam multiplicity.
@item beamed-minimum-free-lengths
List of normal minimum free stem lengths (chord to beams) given beam
multiplicity.
@item beamed-extreme-minimum-free-lengths
List of extreme minimum free stem lengths (chord to beams) given beam
multiplicity.
@item lengths
Default stem lengths.  The list gives a length for each flag count.
@item stem-shorten
How much a stem in a forced direction should be shortened.  The list gives an
amount depending on the number of flags and beams.
@end table
               )",

               /* properties */
               R"(
avoid-note-head
beam
beaming
beamlet-default-length
beamlet-max-length-proportion
default-direction
details
direction
double-stem-separation
duration-log
flag
french-beaming
french-beaming-stem-adjustment
length
length-fraction
max-beam-connect
melody-spanner
neutral-direction
no-stem-extend
note-heads
note-collision-threshold
positioning-done
rests
stem-begin-position
stem-info
stemlet-length
thickness
tremolo-flag
tuplet-start
               )");

/****************************************************************/

Stem_info::Stem_info ()
{
  ideal_y_ = shortest_y_ = 0;
  dir_ = CENTER;
}

void
Stem_info::scale (Real x)
{
  ideal_y_ *= x;
  shortest_y_ *= x;
}
