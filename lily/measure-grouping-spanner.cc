/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "measure-grouping-spanner.hh"

#include "output-def.hh"
#include "spanner.hh"
#include "lookup.hh"
#include "item.hh"
#include "staff-symbol-referencer.hh"

MAKE_SCHEME_CALLBACK (Measure_grouping, print, "ly:measure-grouping::print", 1);
SCM
Measure_grouping::print (SCM grob)
{
  Spanner *me = unsmob<Spanner> (grob);

  SCM which = get_property (me, "style");
  Real height = from_scm<double> (get_property (me, "height"), 1);

  Real t = Staff_symbol_referencer::line_thickness (me)
           * from_scm<double> (get_property (me, "thickness"), 1);
  Grob *common
    = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);

  Real right_point
    = robust_relative_extent (me->get_bound (RIGHT), common, X_AXIS).center ();
  Real left_point = me->get_bound (LEFT)->relative_coordinate (common, X_AXIS);

  Interval iv (left_point, right_point);
  Stencil m;

  /*
    TODO: use line interface
  */
  if (scm_is_eq (which, ly_symbol2scm ("bracket")))
    m = Lookup::bracket (X_AXIS, iv, t, -height, t);
  else if (scm_is_eq (which, ly_symbol2scm ("triangle")))
    m = Lookup::triangle (iv, t, height);

  m.align_to (Y_AXIS, DOWN);
  m.translate_axis (-me->relative_coordinate (common, X_AXIS), X_AXIS);
  return m.smobbed_copy ();
}

ADD_INTERFACE (Measure_grouping,
               R"(
This object indicates groups of beats.  Valid choices for @code{style} are
@code{bracket} and @code{triangle}.
               )",

               /* properties */
               R"(
thickness
style
height
               )");
