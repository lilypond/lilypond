/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2020  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "percent-repeat-item.hh"

#include "item.hh"
#include "font-interface.hh"
#include "lookup.hh"
#include "stream-event.hh"

Stencil
Percent_repeat_item_interface::brew_slash (Grob *me, int count)
{
  Real slope = robust_scm2double (get_property (me, "slope"), 1);
  Real wid = 2.0 / slope;

  /*
    todo: check out if in staff-rule thickness normally.
  */
  Real thick = robust_scm2double (get_property (me, "thickness"), 1);
  Stencil slash = Lookup::repeat_slash (wid, slope, thick);
  Stencil m = slash;

  Real slash_neg_kern
    = robust_scm2double (get_property (me, "slash-negative-kern"), 1.6);
  for (int i = count - 1; i--;)
    m.add_at_edge (X_AXIS, RIGHT, slash, -slash_neg_kern);

  m.translate_axis (-m.extent (Y_AXIS).center (), Y_AXIS);
  return m;
}

Stencil
Percent_repeat_item_interface::x_percent (Grob *me, int count)
{
  Stencil m = brew_slash (me, count);

  Real dot_neg_kern
    = robust_scm2double (get_property (me, "dot-negative-kern"), 0.75);

  Stencil d1 = Font_interface::get_default_font (me)->find_by_name ("dots.dot");
  Stencil d2 = d1;
  d1.translate_axis (0.5, Y_AXIS);
  d2.translate_axis (-0.5, Y_AXIS);

  m.add_at_edge (X_AXIS, LEFT, d1, -dot_neg_kern);
  m.add_at_edge (X_AXIS, RIGHT, d2, -dot_neg_kern);

  return m;
}

MAKE_SCHEME_CALLBACK (Percent_repeat_item_interface, double_percent, 1);
SCM
Percent_repeat_item_interface::double_percent (SCM grob)
{
  Grob *me = unsmob<Grob> (grob);
  Stencil m = x_percent (me, 2);
  m.translate_axis (-m.extent (X_AXIS).center (), X_AXIS);
  return m.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Percent_repeat_item_interface, beat_slash, 1);
SCM
Percent_repeat_item_interface::beat_slash (SCM grob)
{
  Grob *me = unsmob<Grob> (grob);
  Stream_event *cause = me->event_cause ();
  int count = robust_scm2int (get_property (cause, "slash-count"), 1);

  Stencil m;
  if (count == 0)
    m = x_percent (me, 2);
  else
    m = brew_slash (me, count);

  return m.smobbed_copy ();
}

ADD_INTERFACE (Percent_repeat_item_interface,
               "Repeats that look like percent signs.",

               /* properties */
               "dot-negative-kern "
               "slash-negative-kern "
               "slope "
               "thickness "
              );
