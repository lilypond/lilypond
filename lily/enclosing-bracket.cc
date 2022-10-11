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

#include "stencil.hh"
#include "bracket.hh"
#include "grob.hh"
#include "axis-group-interface.hh"
#include "pointer-group-interface.hh"

struct Enclosing_bracket
{

public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM));
};

ADD_INTERFACE (Enclosing_bracket,
               R"(
Brackets alongside bass figures.
               )",

               /* properties */
               R"(
bracket-flare
dashed-edge
edge-height
elements
padding
shorten-pair
thickness
               )");

/* ugh: should make bracket interface. */

MAKE_SCHEME_CALLBACK (Enclosing_bracket, width, "ly:enclosing-bracket::width",
                      1);
SCM
Enclosing_bracket::width (SCM grob)
{
  /*
     UGH. cut & paste code.
  */
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  extract_grob_set (me, "elements", elements);
  if (elements.empty ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Grob *common_x = common_refpoint_of_array (elements, me, X_AXIS);
  Interval xext
    = Axis_group_interface::relative_group_extent (elements, common_x, X_AXIS);

  Stencil left_br = Bracket::make_axis_constrained_bracket (me, 10.0, Y_AXIS,
                                                            LEFT, Interval ());
  Stencil right_br = Bracket::make_axis_constrained_bracket (me, 10.0, Y_AXIS,
                                                             LEFT, Interval ());

  xext.widen (from_scm<double> (get_property (me, "padding"), 0.25));
  left_br.translate_axis (xext[LEFT], X_AXIS);
  right_br.translate_axis (xext[RIGHT], X_AXIS);

  left_br.add_stencil (right_br);
  left_br.translate_axis (-me->relative_coordinate (common_x, X_AXIS), X_AXIS);

  return to_scm (left_br.extent (X_AXIS));
}

MAKE_SCHEME_CALLBACK (Enclosing_bracket, print, "ly:enclosing-bracket::print",
                      1);
SCM
Enclosing_bracket::print (SCM grob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  extract_grob_set (me, "elements", elements);
  if (elements.empty ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Grob *common_x = common_refpoint_of_array (elements, me, X_AXIS);
  Interval xext
    = Axis_group_interface::relative_group_extent (elements, common_x, X_AXIS);
  if (xext.is_empty ())
    {
      me->programming_error ("elements have no X extent.");
      xext = Interval (0, 0);
    }

  Stencil left_br
    = Bracket::make_enclosing_bracket (me, me, elements, Y_AXIS, LEFT);
  Stencil right_br
    = Bracket::make_enclosing_bracket (me, me, elements, Y_AXIS, RIGHT);

  xext.widen (from_scm<double> (get_property (me, "padding"), 0.25));
  left_br.translate_axis (xext[LEFT], X_AXIS);
  right_br.translate_axis (xext[RIGHT], X_AXIS);

  left_br.add_stencil (right_br);
  left_br.translate_axis (-me->relative_coordinate (common_x, X_AXIS), X_AXIS);

  return left_br.smobbed_copy ();
}
