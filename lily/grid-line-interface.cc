/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "grid-line-interface.hh"

#include "grob.hh"
#include "pointer-group-interface.hh"
#include "lookup.hh"
#include "output-def.hh"

MAKE_SCHEME_CALLBACK (Grid_line_interface, print,
                      "ly:grid-line-interface::print", 1);
SCM
Grid_line_interface::print (SCM smobbed_me)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smobbed_me, 1);

  extract_grob_set (me, "elements", elts);
  /* compute common refpoint of elements */
  Grob *refp = common_refpoint_of_array (elts, me, Y_AXIS);
  Interval iv;

  for (vsize i = 0; i < elts.size (); i++)
    {
      Grob *point = elts[i];

      iv.unite (point->extent (refp, Y_AXIS));
    }

  if (iv.is_empty ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real staffline
    = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real thick
    = from_scm<double> (get_property (me, "thickness"), 1.0) * staffline;

  iv += -me->relative_coordinate (refp, Y_AXIS);
  Stencil st = Lookup::filled_box (Box (Interval (0, thick), iv));

  return st.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Grid_line_interface, width,
                      "ly:grid-line-interface::width", 1);
SCM
Grid_line_interface::width (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  Real staffline
    = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real thick
    = from_scm<double> (get_property (me, "thickness"), 1.0) * staffline;

  return to_scm (Interval (0, thick));
}

void
Grid_line_interface::add_grid_point (Grob *me, Grob *b)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("elements"), b);
}

ADD_INTERFACE (Grid_line_interface,
               R"(
A line that is spanned between grid-points.
               )",

               /* properties */
               R"(
elements
thickness
               )");

ADD_INTERFACE (Grid_point_interface,
               R"(
A spanning point for grid lines.
               )",

               /* properties */
               R"(

               )");
