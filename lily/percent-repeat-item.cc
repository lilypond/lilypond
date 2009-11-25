/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2009  Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "lookup.hh"
#include "font-interface.hh"

Stencil
Percent_repeat_item_interface::brew_slash (Grob *me)
{
  Real slope = robust_scm2double (me->get_property ("slope"), 1);
  Real wid = 2.0 / slope;

  /*
    todo: check out if in staff-rule thickness normally.
  */
  Real thick = robust_scm2double (me->get_property ("thickness"), 1);
  Stencil m = Lookup::repeat_slash (wid, slope, thick);
  m.translate_axis (-m.extent (Y_AXIS).center (), Y_AXIS);
  return m;
}

Stencil
Percent_repeat_item_interface::x_percent (Grob *me, int count)
{
  Stencil m;
  Stencil s = brew_slash (me);

  Real dot_neg_kern =
    robust_scm2double (me->get_property ("dot-negative-kern"), 0.75);
  Real slash_neg_kern =
    robust_scm2double (me->get_property ("slash-negative-kern"), 1.6);

  for (int i = count; i--;)
    m.add_at_edge (X_AXIS, RIGHT, s, -slash_neg_kern);
  Stencil d1 = Font_interface::get_default_font (me)->find_by_name ("dots.dot");
  Stencil d2 = d1;
  d1.translate_axis (0.5, Y_AXIS);
  d2.translate_axis (-0.5, Y_AXIS);

  m.add_at_edge (X_AXIS, LEFT, d1, -dot_neg_kern);
  m.add_at_edge (X_AXIS, RIGHT, d2, -dot_neg_kern);

  m.translate_axis (- m.extent (X_AXIS).center (), X_AXIS);
  return m;
}

MAKE_SCHEME_CALLBACK (Percent_repeat_item_interface, double_percent, 1);
SCM
Percent_repeat_item_interface::double_percent (SCM grob)
{
  Grob *me = unsmob_grob (grob);
  Stencil m = x_percent (me, 2);
  return m.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Percent_repeat_item_interface, beat_slash, 1);
SCM
Percent_repeat_item_interface::beat_slash (SCM grob)
{
  Grob *me = unsmob_grob (grob);
  Stencil m = brew_slash (me);

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

