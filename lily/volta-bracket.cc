/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "warn.hh"
#include "font-interface.hh"
#include "line-interface.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "text-interface.hh"
#include "volta-bracket.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "directional-element-interface.hh"
#include "lookup.hh"
#include "bracket.hh"
#include "lily-imports.hh"

#include <cstring>

using std::string;

/*
  this is too complicated. Yet another version of side-positioning,
  badly implemented.

  --

  * Should look for system_start_delim to find left edge of staff.
  */

MAKE_SCHEME_CALLBACK (Volta_bracket_interface, print,
                      "ly:volta-bracket-interface::print", 1);
SCM
Volta_bracket_interface::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  Spanner *orig_span = me->original ();
  bool broken_first_bracket = orig_span && (orig_span->broken_intos_[0] == me);

  Output_def *layout = me->layout ();

  Item *bound = me->get_bound (LEFT);

  /*
    If the volta bracket appears after a line-break, make
    it start after the prefatory matter.
  */
  Real left = 0.;
  if (bound->break_status_dir () == RIGHT)
    {
      Paper_column *pc = bound->get_column ();
      left
        = pc->break_align_width (pc, ly_symbol2scm ("break-alignment"))[RIGHT]
          // For some reason, break_align_width is relative to
          // the x-parent of the column.
          - bound->relative_coordinate (pc->get_x_parent (), X_AXIS);
    }
  else
    {
      /*
        the volta spanner is attached to the bar-line, which is moved
        to the right. We don't need to compensate for the left edge.
      */
    }

  modify_edge_height (me);
  if (!me->is_live ())
    return SCM_EOL;

  Drul_array<Real> edge_height
    = from_scm (get_property (me, "edge-height"), Drul_array<Real> (1.0, 1.0));
  Drul_array<Real> flare = from_scm (get_property (me, "bracket-flare"),
                                     Drul_array<Real> (0.0, 0.0));
  Drul_array<Real> shorten
    = from_scm (get_property (me, "shorten-pair"), Drul_array<Real> (0.0, 0.0));

  scale_drul (&edge_height, -get_grob_direction (me));

  Interval empty;
  Offset start;
  start[X_AXIS] = me->spanner_length () - left;

  Stencil total = Bracket::make_bracket (me, Y_AXIS, start, edge_height, empty,
                                         flare, shorten);

  if (!orig_span || broken_first_bracket)
    {
      SCM text = get_property (me, "text");
      SCM properties = me->get_property_alist_chain (SCM_EOL);
      auto num = Text_interface::interpret_markup (layout, properties, text);
      num.align_to (Y_AXIS, UP);
      num.translate_axis (-0.5, Y_AXIS);
      total.add_at_edge (X_AXIS, LEFT, num,
                         -num.extent (X_AXIS).length () - 1.0);
    }

  total.translate_axis (left, X_AXIS);
  return total.smobbed_copy ();
}

void
Volta_bracket_interface::modify_edge_height (Spanner *me)
{
  Spanner *orig_span = me->original ();

  bool broken_first_bracket = orig_span && (orig_span->broken_intos_[0] == me);
  bool broken_last_bracket
    = orig_span && (orig_span->broken_intos_.back () == me);
  bool no_vertical_start = orig_span && !broken_first_bracket;
  bool no_vertical_end = orig_span && !broken_last_bracket;

  if (no_vertical_end || no_vertical_start)
    {
      Drul_array<Real> edge_height = from_scm (get_property (me, "edge-height"),
                                               Drul_array<Real> (1.0, 1.0));
      if (no_vertical_start)
        edge_height[LEFT] = 0.0;

      if (no_vertical_end)
        edge_height[RIGHT] = 0.0;

      set_property (me, "edge-height", to_scm (edge_height));
    }

  if (broken_last_bracket && no_vertical_end && no_vertical_start
      && !broken_first_bracket)
    me->suicide ();
}

void
Volta_bracket_interface::add_bar (Spanner *me, Item *b)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("bars"), b);
  add_bound_item (me, b);
}

ADD_INTERFACE (Volta_bracket_interface,
               R"(
Volta bracket with number.
               )",

               /* properties */
               R"(
bars
dashed-edge
height
shorten-pair
thickness
               )");
