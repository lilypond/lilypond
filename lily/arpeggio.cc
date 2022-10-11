/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "arpeggio.hh"

#include "bezier.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "international.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "stem.hh"
#include "warn.hh"

static Stencil
get_squiggle (Grob *me)
{
  Font_metric *fm = Font_interface::get_default_font (me);
  Stencil squiggle = fm->find_by_name ("scripts.arpeggio");

  return squiggle;
}

Grob *
Arpeggio::get_common_y (Grob *me)
{
  Grob *common = me;

  extract_grob_set (me, "stems", stems);
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *stem = stems[i];
      common = common->common_refpoint (
        Staff_symbol_referencer::get_staff_symbol (stem), Y_AXIS);
    }

  return common;
}

MAKE_SCHEME_CALLBACK (Arpeggio, calc_cross_staff,
                      "ly:arpeggio::calc-cross-staff", 1);
SCM
Arpeggio::calc_cross_staff (SCM grob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);

  extract_grob_set (me, "stems", stems);
  Grob *vag = 0;

  for (vsize i = 0; i < stems.size (); i++)
    {
      if (!i)
        vag = Grob::get_vertical_axis_group (stems[i]);
      else
        {
          if (vag != Grob::get_vertical_axis_group (stems[i]))
            return SCM_BOOL_T;
        }
    }

  return SCM_BOOL_F;
}

MAKE_SCHEME_CALLBACK (Arpeggio, calc_positions, "ly:arpeggio::calc-positions",
                      1);
SCM
Arpeggio::calc_positions (SCM grob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  Grob *common = get_common_y (me);

  /*
    TODO:

    Using stems here is not very convenient; should store noteheads
    instead, and also put them into the support. Now we will mess up
    in vicinity of a collision.
  */
  Interval heads;
  Real my_y = me->relative_coordinate (common, Y_AXIS);

  extract_grob_set (me, "stems", stems);
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *stem = stems[i];
      Grob *ss = Staff_symbol_referencer::get_staff_symbol (stem);
      Interval iv = Stem::head_positions (stem);
      iv *= Staff_symbol_referencer::staff_space (me) / 2.0;
      Real staff_y = ss ? ss->relative_coordinate (common, Y_AXIS) : 0.0;
      heads.unite (iv + staff_y - my_y);
    }

  heads *= 1 / Staff_symbol_referencer::staff_space (me);

  return to_scm (heads);
}

MAKE_SCHEME_CALLBACK (Arpeggio, print, "ly:arpeggio::print", 1);
SCM
Arpeggio::print (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Interval heads = from_scm (get_property (me, "positions"), Interval ())
                   * Staff_symbol_referencer::staff_space (me);

  if (heads.is_empty () || heads.length () < 0.5)
    {
      if (from_scm<bool> (get_property (me, "transparent")))
        {
          /*
            This is part of a cross-staff/-voice span-arpeggio,
            so we need to ensure `heads' is large enough to encompass
            a single trill-element since the span-arpeggio depends on
            its children to prevent collisions.
          */
          heads.unite (get_squiggle (me).extent (Y_AXIS));
        }
      else
        {
          me->warning (_ ("no heads for arpeggio found?"));
          me->suicide ();
          return SCM_EOL;
        }
    }

  SCM ad = get_property (me, "arpeggio-direction");
  Direction dir = CENTER;
  if (is_scm<Direction> (ad))
    dir = from_scm<Direction> (ad);

  Stencil mol;
  Stencil squiggle (get_squiggle (me));

  /*
    Compensate for rounding error which may occur when a chord
    reaches the center line, resulting in an extra squiggle
    being added to the arpeggio stencil.  This value is appreciably
    larger than the rounding error, which is in the region of 1e-16
    for a global-staff-size of 20, but small enough that it does not
    interfere with smaller staff sizes.
  */
  const Real epsilon = 1e-3;

  Stencil arrow;
  if (dir)
    {
      Font_metric *fm = Font_interface::get_default_font (me);
      arrow
        = fm->find_by_name ("scripts.arpeggio.arrow." + std::to_string (dir));
      heads[dir] -= dir * arrow.extent (Y_AXIS).length ();
    }

  while (mol.extent (Y_AXIS).length () + epsilon < heads.length ())
    mol.add_at_edge (Y_AXIS, UP, squiggle, 0.0);

  mol.translate_axis (heads[LEFT], Y_AXIS);
  if (dir)
    mol.add_at_edge (Y_AXIS, dir, arrow, 0);

  return mol.smobbed_copy ();
}

/* Draws a vertical bracket to the left of a chord
   Chris Jackson <chris@fluffhouse.org.uk> */

MAKE_SCHEME_CALLBACK (Arpeggio, brew_chord_bracket,
                      "ly:arpeggio::brew-chord-bracket", 1);
SCM
Arpeggio::brew_chord_bracket (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Interval heads = from_scm (get_property (me, "positions"), Interval ())
                   * Staff_symbol_referencer::staff_space (me);

  Real th = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"))
            * from_scm<double> (get_property (me, "thickness"), 1);
  Real sp = 1.5 * Staff_symbol_referencer::staff_space (me);
  Real dy = heads.length () + sp;
  Real x = from_scm<double> (get_property (me, "protrusion"), 0.4);

  Stencil mol (Lookup::bracket (Y_AXIS, Interval (0, dy), th, x, th));
  mol.translate_axis (heads[LEFT] - sp / 2.0, Y_AXIS);
  return mol.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Arpeggio, brew_chord_slur, "ly:arpeggio::brew-chord-slur",
                      1);
SCM
Arpeggio::brew_chord_slur (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  SCM dash_definition = get_property (me, "dash-definition");
  Interval heads = from_scm (get_property (me, "positions"), Interval ())
                   * Staff_symbol_referencer::staff_space (me);

  Real lt = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"))
            * from_scm<double> (get_property (me, "line-thickness"), 1.0);
  Real th = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"))
            * from_scm<double> (get_property (me, "thickness"), 1.0);
  Real dy = heads.length ();

  Real height_limit = 1.5;
  Real ratio = .33;
  Bezier curve = slur_shape (dy, height_limit, ratio);
  curve.rotate (90.0);

  Stencil mol (Lookup::slur (curve, th, lt, dash_definition));
  mol.translate_axis (heads[LEFT], Y_AXIS);
  return mol.smobbed_copy ();
}

/*
  We have to do a callback, because print () triggers a
  vertical alignment if it is cross-staff.
*/
MAKE_SCHEME_CALLBACK (Arpeggio, width, "ly:arpeggio::width", 1);
SCM
Arpeggio::width (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (get_squiggle (me).extent (X_AXIS));
}

MAKE_SCHEME_CALLBACK (Arpeggio, pure_height, "ly:arpeggio::pure-height", 3);
SCM
Arpeggio::pure_height (SCM smob, SCM, SCM)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  if (from_scm<bool> (get_property (me, "cross-staff")))
    return to_scm (Interval ());

  return Grob::stencil_height (smob);
}

ADD_INTERFACE (Arpeggio,
               R"(
Functions and settings for drawing an arpeggio symbol.
               )",

               /* properties */
               R"(
arpeggio-direction
dash-definition
line-thickness
positions
protrusion
script-priority
stems
thickness
               )");
