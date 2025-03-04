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

bool
Script_interface::script_priority_less (Grob *g1, Grob *g2)
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
grob-defaults
positioning-done
script-column
script-priority
script-stencil
side-relative-direction
slur
slur-padding
staff-position
toward-stem-shift
toward-stem-shift-in-column
               )");
