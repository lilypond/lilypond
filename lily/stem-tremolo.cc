/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "stem-tremolo.hh"

#include "spanner.hh"
#include "beam.hh"
#include "directional-element-interface.hh"
#include "international.hh"
#include "item.hh"
#include "lookup.hh"
#include "note-collision.hh"
#include "note-column.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "warn.hh"

using std::vector;

MAKE_SCHEME_CALLBACK (Stem_tremolo, calc_cross_staff,
                      "ly:stem-tremolo::calc-cross-staff", 1)
SCM
Stem_tremolo::calc_cross_staff (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = unsmob<Grob> (get_object (me, "stem"));
  return get_property (stem, "cross-staff");
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, calc_slope, "ly:stem-tremolo::calc-slope",
                      1)
SCM
Stem_tremolo::calc_slope (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = unsmob<Grob> (get_object (me, "stem"));
  Spanner *beam = Stem::get_beam (stem);

  SCM style = get_property (me, "style");

  if (beam && !scm_is_eq (style, ly_symbol2scm ("constant")))
    {
      Real dy = 0;
      SCM s = get_property (beam, "quantized-positions");
      if (is_number_pair (s))
        dy = -from_scm<double> (scm_car (s)) + from_scm<double> (scm_cdr (s));

      Grob *s2 = Beam::last_normal_stem (beam);
      Grob *s1 = Beam::first_normal_stem (beam);

      Grob *common = s1->common_refpoint (s2, X_AXIS);
      Real dx = s2->relative_coordinate (common, X_AXIS)
                - s1->relative_coordinate (common, X_AXIS);

      return to_scm (dx ? dy / dx : 0);
    }
  else
    /* down stems with flags should have more sloped trems (helps avoid
       flag/stem collisions without making the stem very long) */
    return to_scm ((Stem::duration_log (stem) >= 3
                    && get_grob_direction (me) == DOWN && !beam)
                     ? 0.40
                     : 0.25);
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, calc_width, "ly:stem-tremolo::calc-width",
                      1)
SCM
Stem_tremolo::calc_width (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = unsmob<Grob> (get_object (me, "stem"));
  Direction dir = get_grob_direction (me);
  bool beam = Stem::get_beam (stem);
  bool flag = Stem::duration_log (stem) >= 3 && !beam;

  /* beamed stems and up-stems with flags have shorter tremolos */
  return to_scm (((dir == UP && flag) || beam) ? 1.0 : 1.5);
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, calc_shape, "ly:stem-tremolo::calc-shape",
                      1)
SCM
Stem_tremolo::calc_shape (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = unsmob<Grob> (get_object (me, "stem"));
  Direction dir = get_grob_direction (me);
  bool beam = Stem::get_beam (stem);
  bool flag = Stem::duration_log (stem) >= 3 && !beam;
  SCM style = get_property (me, "style");

  return ly_symbol2scm (!scm_is_eq (style, ly_symbol2scm ("constant"))
                            && ((dir == UP && flag) || beam)
                          ? "rectangle"
                          : "beam-like");
}

Real
Stem_tremolo::get_beam_translation (Grob *me)
{
  Grob *stem = unsmob<Grob> (get_object (me, "stem"));
  Spanner *beam = Stem::get_beam (stem);

  return (beam && beam->is_live ())
           ? Beam::get_beam_translation (beam)
           : (Staff_symbol_referencer::staff_space (me)
              * from_scm<double> (get_property (me, "length-fraction"), 1.0)
              * 0.81);
}

Stencil
Stem_tremolo::raw_stencil (Grob *me, Real slope, Direction dir)
{
  Real ss = Staff_symbol_referencer::staff_space (me);
  Real thick = from_scm<double> (get_property (me, "beam-thickness"), 1);
  Real width = from_scm<double> (get_property (me, "beam-width"), 1);
  Real blot = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));
  SCM shape = get_property (me, "shape");
  if (!scm_is_symbol (shape))
    shape = ly_symbol2scm ("beam-like");

  width *= ss;
  thick *= ss;

  Stencil a;
  if (scm_is_eq (shape, ly_symbol2scm ("rectangle")))
    a = Lookup::rotated_box (slope, width, thick, blot);
  else
    a = Lookup::beam (slope, width, thick, blot);

  a.align_to (X_AXIS, CENTER);
  a.align_to (Y_AXIS, CENTER);

  int tremolo_flags = from_scm (get_property (me, "flag-count"), 0);
  if (!tremolo_flags)
    {
      programming_error ("no tremolo flags");

      me->suicide ();
      return Stencil ();
    }

  Real beam_translation = get_beam_translation (me);

  Stencil mol;
  for (int i = 0; i < tremolo_flags; i++)
    {
      Stencil b (a);
      b.translate_axis (beam_translation * i * dir * -1, Y_AXIS);
      mol.add_stencil (b);
    }
  return mol;
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, pure_height, "ly:stem-tremolo::pure-height",
                      3);
SCM
Stem_tremolo::pure_height (SCM smob, SCM, SCM)
{
  Item *me = unsmob<Item> (smob);

  /*
    Cannot use the real slope, since it looks at the Beam.
   */
  Stencil s1 (untranslated_stencil (me, 0.35));
  Item *stem = unsmob<Item> (get_object (me, "stem"));
  if (!stem)
    return to_scm (s1.extent (Y_AXIS));

  Direction dir = get_grob_direction (me);

  Spanner *beam = Stem::get_beam (stem);

  if (!beam)
    return to_scm (s1.extent (Y_AXIS));

  Interval ph = stem->pure_y_extent (stem, 0, INT_MAX);
  if (ph.is_empty ()) // This should not really happen but does
    return to_scm (s1.extent (Y_AXIS));

  Stem_info si = Stem::get_stem_info (stem);
  ph[-dir] = si.shortest_y_;
  if (ph.is_empty ()) // This should not really happen either
    return to_scm (s1.extent (Y_AXIS));

  int beam_count = Stem::beam_multiplicity (stem).length () + 1;
  Real beam_translation = get_beam_translation (me);

  ph = ph - dir * std::max (beam_count, 1) * beam_translation;
  ph = ph - ph.center (); // TODO: this nullifies the previous line?!?

  return to_scm (ph);
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, width, "ly:stem-tremolo::width", 1);
SCM
Stem_tremolo::width (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  /*
    Cannot use the real slope, since it looks at the Beam.
   */
  Stencil s1 (untranslated_stencil (me, 0.35));

  return to_scm (s1.extent (X_AXIS));
}

Real
Stem_tremolo::vertical_length (Grob *me)
{
  return untranslated_stencil (me, 0.35).extent (Y_AXIS).length ();
}

Stencil
Stem_tremolo::untranslated_stencil (Grob *me, Real slope)
{
  Grob *stem = unsmob<Grob> (get_object (me, "stem"));
  if (!stem)
    {
      programming_error ("no stem for stem-tremolo");
      return Stencil ();
    }

  Direction dir = get_grob_direction (me);

  bool whole_note = Stem::duration_log (stem) <= 0;

  /* for a whole note, we position relative to the notehead, so we want the
     stencil aligned on the flag closest to the head */
  Direction stencil_dir = whole_note ? -dir : dir;
  return raw_stencil (me, slope, stencil_dir);
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, calc_y_offset,
                      "ly:stem-tremolo::calc-y-offset", 1);
SCM
Stem_tremolo::calc_y_offset (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (y_offset (me, false));
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, pure_calc_y_offset,
                      "ly:stem-tremolo::pure-calc-y-offset", 3);
SCM
Stem_tremolo::pure_calc_y_offset (SCM smob, SCM, /* start */
                                  SCM /* end */)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (y_offset (me, true));
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, calc_direction,
                      "ly:stem-tremolo::calc-direction", 1);
SCM
Stem_tremolo::calc_direction (SCM smob)
{
  Item *me = unsmob<Item> (smob);

  Item *stem = unsmob<Item> (get_object (me, "stem"));
  if (!stem)
    return to_scm (CENTER);

  Direction stemdir = get_grob_direction (stem);

  vector<int> nhp = Stem::note_head_positions (stem);
  /*
   * We re-decide stem-dir if there may be collisions with other
   * note heads in the staff.
   */
  Grob *maybe_nc = stem->get_x_parent ()->get_x_parent ();
  bool whole_note = Stem::duration_log (stem) <= 0;
  if (whole_note && has_interface<Note_collision_interface> (maybe_nc))
    {
      Drul_array<bool> avoid_me;
      vector<int> all_nhps
        = Note_collision_interface::note_head_positions (maybe_nc);
      if (all_nhps[0] < nhp[0])
        avoid_me[DOWN] = true;
      if (all_nhps.back () > nhp.back ())
        avoid_me[UP] = true;
      if (avoid_me[stemdir])
        {
          stemdir = -stemdir;
          if (avoid_me[stemdir])
            {
              me->warning (
                _ ("Whole-note tremolo may collide with simultaneous notes."));
              stemdir = -stemdir;
            }
        }
    }
  return to_scm (stemdir);
}

Real
Stem_tremolo::y_offset (Grob *me, bool pure)
{
  Item *stem = unsmob<Item> (get_object (me, "stem"));
  if (!stem)
    return 0.0;

  Direction dir = get_grob_direction (me);

  Spanner *beam = Stem::get_beam (stem);
  Real beam_translation = get_beam_translation (me);

  int beam_count = beam ? (Stem::beam_multiplicity (stem).length () + 1) : 0;

  if (pure && beam)
    {
      Interval ph = stem->pure_y_extent (stem, 0, INT_MAX);
      Stem_info si = Stem::get_stem_info (stem);
      ph[-dir] = si.shortest_y_;

      return (ph - dir * std::max (beam_count, 1) * beam_translation)[dir]
             - dir * 0.5 * me->pure_y_extent (me, 0, INT_MAX).length ();
    }

  Real end_y = (pure ? stem->pure_y_extent (stem, 0, INT_MAX)[dir]
                     : stem->extent (stem, Y_AXIS)[dir])
               - dir * std::max (beam_count, 1) * beam_translation
               - Stem::beam_end_corrective (stem);

  if (!beam && Stem::duration_log (stem) >= 3)
    {
      end_y -= dir * (Stem::duration_log (stem) - 2) * beam_translation;
      if (dir == UP)
        end_y -= dir * beam_translation * 0.5;
    }

  bool whole_note = Stem::duration_log (stem) <= 0;
  if (whole_note || std::isinf (end_y))
    {
      /* we shouldn't position relative to the end of the stem since the stem
         is invisible */
      Real ss = Staff_symbol_referencer::staff_space (me);
      vector<int> nhp = Stem::note_head_positions (stem);
      if (nhp.empty ())
        {
          me->warning (_ ("stem tremolo has no note heads"));
          end_y = 0.0;
        }
      else
        {
          Real note_head = (dir == UP ? nhp.back () : nhp[0]) * ss / 2;
          end_y = note_head + dir * 1.5;
        }
    }

  return end_y;
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, print, "ly:stem-tremolo::print", 1);
SCM
Stem_tremolo::print (SCM grob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);

  Stencil s = untranslated_stencil (
    me, from_scm<double> (get_property (me, "slope"), 0.25));
  return s.smobbed_copy ();
}

ADD_INTERFACE (Stem_tremolo,
               R"(
A beam slashing a stem to indicate a tremolo.  The property @code{shape} can be
@code{beam-like} or @code{rectangle}.
               )",

               /* properties */
               R"(
beam-thickness
beam-width
direction
flag-count
length-fraction
stem
shape
slope
               )");
