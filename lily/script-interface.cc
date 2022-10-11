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

#include "script-interface.hh"

#include "directional-element-interface.hh"
#include "item.hh"
#include "warn.hh"
#include "international.hh"
#include "font-interface.hh"
#include "side-position-interface.hh"
#include "output-def.hh"
#include "lookup.hh"
#include "stem.hh"
#include "note-column.hh"

Stencil
Script_interface::get_stencil (Grob *me, Direction d)
{
  SCM s = get_property (me, "script-stencil");

  if (!scm_is_pair (s))
    {
      warning (_f ("script-stencil property must be pair: %s",
                   ly_scm_write_string (s)));
      return Stencil ();
    }

  SCM key = scm_car (s);
  if (scm_is_eq (key, ly_symbol2scm ("feta")))
    {
      SCM name_entry = scm_cdr (s);
      SCM str;
      if (scm_is_pair (name_entry))
        {
          if (d)
            str = index_get_cell (name_entry, d);
          else
            {
              if (!ly_is_equal (scm_car (name_entry), scm_cdr (name_entry)))
                {
                  me->warning (
                    _f ("script needs an explicit direction specifier"
                        " to disambiguate between different glyphs"));
                }
              str = scm_car (name_entry);
            }
        }
      else
        str = name_entry;
      return Font_interface::get_default_font (me)->find_by_name (
        "scripts." + ly_scm2string (str));
    }
  else
    {
      programming_error (
        "cannot deal with script-stencil key other than 'feta");
      return Stencil ();
    }
}

MAKE_SCHEME_CALLBACK (Script_interface, calc_positioning_done,
                      "ly:script-interface::calc-positioning-done", 1);
SCM
Script_interface::calc_positioning_done (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  if (Grob *par = me->get_x_parent ())
    {
      Grob *stem = Note_column::get_stem (par);
      if (stem && Stem::first_head (stem))
        me->set_x_parent (Stem::first_head (stem));
    }
  return SCM_BOOL_T;
}

Direction
Script_interface::get_direction (Grob *me)
{
  SCM reldir = get_property (me, "side-relative-direction");
  const auto relative_dir = from_scm<Direction> (reldir, Direction (1));

  SCM other_elt = get_object (me, "direction-source");
  if (auto *e = unsmob<Grob> (other_elt))
    return relative_dir * get_grob_direction (e);

  /* This might be a script attached to a skip in a
     Dynamics context between two piano staves, for
     example.  Some scripts have differing glyphs depending
     on their direction, so we still pass a non-CENTER
     direction for downstream code, but we pick it silently
     because it can be useful, and let the user write an
     explicit direction specifier if the result does not fit. */
  return CENTER;
}

MAKE_SCHEME_CALLBACK (Script_interface, calc_direction,
                      "ly:script-interface::calc-direction", 1);
SCM
Script_interface::calc_direction (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Direction d = Script_interface::get_direction (me);
  (void) get_property (me, "positioning-done");
  return to_scm (d);
}

MAKE_SCHEME_CALLBACK (Script_interface, calc_cross_staff,
                      "ly:script-interface::calc-cross-staff", 1);
SCM
Script_interface::calc_cross_staff (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = Note_column::get_stem (me->get_x_parent ());

  if (stem && from_scm<bool> (get_property (stem, "cross-staff")))
    return SCM_BOOL_T;

  Grob *slur = unsmob<Grob> (get_object (me, "slur"));
  SCM avoid_slur = get_property (me, "avoid-slur");
  if (slur && from_scm<bool> (get_property (slur, "cross-staff"))
      && (scm_is_eq (avoid_slur, ly_symbol2scm ("outside"))
          || scm_is_eq (avoid_slur, ly_symbol2scm ("around"))))
    return SCM_BOOL_T;

  return SCM_BOOL_F;
}

MAKE_SCHEME_CALLBACK (Script_interface, print, "ly:script-interface::print", 1);

SCM
Script_interface::print (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  Direction dir = get_grob_direction (me);

  return get_stencil (me, dir).smobbed_copy ();
}

bool
Script_interface::script_priority_less (const Grob *g1, const Grob *g2)
{
  SCM script_priority_sym = ly_symbol2scm ("script-priority");
  SCM p1 = get_property (g1, script_priority_sym);
  SCM p2 = get_property (g2, script_priority_sym);
  return from_scm<int> (p1) < from_scm<int> (p2);
}

struct Text_script
{
};

ADD_INTERFACE (Text_script,
               R"(
An object that is put above or below a note.
               )",

               /* properties */
               R"(
avoid-slur
script-priority
slur
               )");

/*
  Hmm. Where should we put add-stem-support ?
*/
ADD_INTERFACE (Script_interface,
               R"(
An object that is put above or below a note.
               )",

               /* properties */
               R"(
avoid-slur
direction-source
positioning-done
script-column
script-priority
script-stencil
side-relative-direction
slur
slur-padding
toward-stem-shift
toward-stem-shift-in-column
               )");
