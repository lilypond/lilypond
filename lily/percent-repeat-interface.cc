/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "grob-interface.hh"
#include "font-interface.hh"
#include "item.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"

class Percent_repeat_interface
{
public:
  DECLARE_SCHEME_CALLBACK (beat_slash, (SCM));
  DECLARE_SCHEME_CALLBACK (percent, (SCM));
  DECLARE_SCHEME_CALLBACK (double_percent, (SCM));
  static Stencil x_percent (Grob *, int);
  static Stencil brew_slash (Grob *, int);
};

Stencil
Percent_repeat_interface::brew_slash (Grob *me, int count)
{
  /* Scale everything by staff-space, don't scale thickness by line-thickness.
   The reason is that line-thickness is more to control the thickness of thin
   lines, which should not get too thin with small staff sizes.  Consequently,
   staff-space and line-thickness are not always proportional.  However, percent
   repeat signs should have the same proportions at all staff sizes. */
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real slope = from_scm<double> (get_property (me, "slope"), 1);
  Real wid = 2.0 / slope * staff_space;
  Real thick
    = from_scm<double> (get_property (me, "thickness"), 1) * staff_space;
  Stencil slash = Lookup::repeat_slash (wid, slope, thick);
  Stencil m = slash;

  Real slash_neg_kern
    = from_scm<double> (get_property (me, "slash-negative-kern"), 1.6)
      * staff_space;
  for (int i = count - 1; i--;)
    m.add_at_edge (X_AXIS, RIGHT, slash, -slash_neg_kern);

  m.align_to (Y_AXIS, CENTER);
  return m;
}

Stencil
Percent_repeat_interface::x_percent (Grob *me, int count)
{
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Stencil m = brew_slash (me, count);

  Real dot_neg_kern
    = from_scm<double> (get_property (me, "dot-negative-kern"), 0.75)
      * staff_space;

  Stencil d1 = Font_interface::get_default_font (me)->find_by_name ("dots.dot");
  Stencil d2 = d1;
  d1.translate_axis (0.5 * staff_space, Y_AXIS);
  d2.translate_axis (-0.5 * staff_space, Y_AXIS);

  m.add_at_edge (X_AXIS, LEFT, d1, -dot_neg_kern);
  m.add_at_edge (X_AXIS, RIGHT, d2, -dot_neg_kern);
  return m;
}

MAKE_SCHEME_CALLBACK (Percent_repeat_interface, percent,
                      "ly:percent-repeat-interface::percent", 1);
SCM
Percent_repeat_interface::percent (SCM grob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  Stencil m = x_percent (me, 1);
  return m.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Percent_repeat_interface, double_percent,
                      "ly:percent-repeat-interface::double-percent", 1);
SCM
Percent_repeat_interface::double_percent (SCM grob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  Stencil m = x_percent (me, 2);
  m.align_to (X_AXIS, CENTER);
  return m.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Percent_repeat_interface, beat_slash,
                      "ly:percent-repeat-interface::beat-slash", 1);
SCM
Percent_repeat_interface::beat_slash (SCM grob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  Stream_event *cause = me->event_cause ();
  int count = from_scm (get_property (cause, "slash-count"), 1);

  Stencil m;
  if (count == 0)
    m = x_percent (me, 2);
  else
    m = brew_slash (me, count);

  return m.smobbed_copy ();
}

ADD_INTERFACE (Percent_repeat_interface,
               R"(
Repeats that look like percent signs.
               )",

               /* properties */
               R"(
dot-negative-kern
slash-negative-kern
slope
thickness
               )");
