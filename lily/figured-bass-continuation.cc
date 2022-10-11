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

#include "line-interface.hh"
#include "spanner.hh"
#include "output-def.hh"
#include "item.hh"
#include "stencil.hh"
#include "pointer-group-interface.hh"
#include "axis-group-interface.hh"

#include "horizontal-bracket.hh"

struct Figured_bass_continuation
{

public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (center_on_figures, (SCM));
};

MAKE_SCHEME_CALLBACK (Figured_bass_continuation, center_on_figures,
                      "ly:figured-bass-continuation::center-on-figures", 1);
SCM
Figured_bass_continuation::center_on_figures (SCM grob)
{
  Spanner *me = unsmob<Spanner> (grob);
  extract_grob_set (me, "figures", figures);
  if (figures.empty ())
    return to_scm (0.0);
  Grob *common = common_refpoint_of_array (figures, me, Y_AXIS);

  Interval ext
    = Axis_group_interface::relative_group_extent (figures, common, Y_AXIS);
  if (ext.is_empty ())
    return to_scm (0.0);
  return to_scm (ext.center () - me->relative_coordinate (common, Y_AXIS));
}

MAKE_SCHEME_CALLBACK (Figured_bass_continuation, print,
                      "ly:figured-bass-continuation::print", 1);
SCM
Figured_bass_continuation::print (SCM grob)
{
  Spanner *me = unsmob<Spanner> (grob);

  Real thick = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"))
               * from_scm<double> (get_property (me, "thickness"), 1);

  Interval spanned;

  Grob *common
    = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);
  for (const auto d : {LEFT, RIGHT})
    {
      Item *bound = me->get_bound (d);
      Direction extdir
        = (d == LEFT && from_scm<bool> (get_property (bound, "implicit")))
            ? LEFT
            : RIGHT;

      spanned[d] = robust_relative_extent (bound, common, X_AXIS)[extdir]
                   - me->relative_coordinate (common, X_AXIS);
    }
  spanned.widen (-from_scm<double> (get_property (me, "padding"), 0.2));

  Stencil extender;
  if (!spanned.is_empty ())
    extender = Line_interface::make_line (thick, Offset (spanned[LEFT], 0),
                                          Offset (spanned[RIGHT], 0));

  return extender.smobbed_copy ();
}

ADD_INTERFACE (Figured_bass_continuation,
               R"(
Simple extender line between bounds.
               )",

               /* properties */
               R"(
thickness
padding
figures
               )");
