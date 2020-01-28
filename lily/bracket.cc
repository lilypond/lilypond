/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "bracket.hh"

#include "axis-group-interface.hh"
#include "directional-element-interface.hh"
#include "grob.hh"
#include "item.hh"
#include "line-interface.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"

using std::vector;

/*
  should move to lookup?

  TODO: this will fail for very short (shorter than the flare)
  brackets.
*/
Stencil
Bracket::make_bracket (Grob *me, // for line properties.
                       Axis protrusion_axis, Offset dz, Drul_array<Real> height,
                       Interval gap, Drul_array<Real> flare,
                       Drul_array<Real> shorten)
{
  Drul_array<Offset> corners (Offset (0, 0), dz);

  Real length = dz.length ();
  Drul_array<Offset> gap_corners;

  Axis bracket_axis = other_axis (protrusion_axis);

  Drul_array<Offset> straight_corners = corners;

  for (LEFT_and_RIGHT (d))
    straight_corners[d] += -d * shorten[d] / length * dz;

  if (!gap.is_empty ())
    {
      for (LEFT_and_RIGHT (d))
        gap_corners[d] = (dz * 0.5) + gap[d] / length * dz;
    }

  Drul_array<Offset> flare_corners = straight_corners;
  for (LEFT_and_RIGHT (d))
    {
      flare_corners[d][bracket_axis] = straight_corners[d][bracket_axis];
      flare_corners[d][protrusion_axis] += height[d];
      straight_corners[d][bracket_axis] += -d * flare[d];
    }

  Stencil m;
  if (!gap.is_empty ())
    for (LEFT_and_RIGHT (d))
      m.add_stencil (
          Line_interface::line (me, straight_corners[d], gap_corners[d]));
  else
    m.add_stencil (Line_interface::line (me, straight_corners[LEFT],
                                         straight_corners[RIGHT]));

  if (scm_is_eq (me->get_property ("style"), ly_symbol2scm ("dashed-line"))
      && !to_boolean (me->get_property ("dashed-edge")))
    me->set_property ("style", ly_symbol2scm ("line"));
  for (LEFT_and_RIGHT (d))
    m.add_stencil (
        Line_interface::line (me, straight_corners[d], flare_corners[d]));
  return m;
}

/*
  Return a bracket oriented along either the X- or Y-axis.  Passing
  Interval () for gap creates an unbroken bracket.
*/
Stencil
Bracket::make_axis_constrained_bracket (Grob *me, Real length, Axis a,
                                        Direction dir, Interval gap)
{
  Drul_array<Real> edge_height = robust_scm2interval (
      me->get_property ("edge-height"), Interval (1.0, 1.0));
  Drul_array<Real> flare = robust_scm2interval (
      me->get_property ("bracket-flare"), Interval (0, 0));
  Drul_array<Real> shorten = robust_scm2interval (
      me->get_property ("shorten-pair"), Interval (0, 0));

  // Make sure that it points in the correct direction:
  scale_drul (&edge_height, Real (-dir));

  Offset start;
  start[a] = length;

  Drul_array<bool> connect_to_other
      = robust_scm2booldrul (me->get_property ("connect-to-neighbor"),
                             Drul_array<bool> (false, false));

  for (LEFT_and_RIGHT (d))
    {
      if (connect_to_other[d])
        {
          edge_height[d] = 0.0;
          flare[d] = 0.0;
          shorten[d] = 0.0;
        }
    }

  return make_bracket (me, other_axis (a), start, edge_height, gap, flare,
                       shorten);
}

/*
  Return an axis-constrained, ungapped bracket which encloses a group of
  grobs.  Used for analysis brackets (HorizontalBracket) and
  figured bass (BassFigureBracket).
*/
Stencil
Bracket::make_enclosing_bracket (Grob *me, Grob *refpoint, vector<Grob *> grobs,
                                 Axis a, Direction dir)
{
  Grob *common = common_refpoint_of_array (grobs, refpoint, a);
  Interval ext = Axis_group_interface::relative_group_extent (grobs, common, a);

  if (ext.is_empty ())
    {
      me->programming_error ("Can't enclose empty extents with bracket");
      return Stencil ();
    }
  else
    {
      Stencil b = make_axis_constrained_bracket (me, ext.length (), a, dir,
                                                 Interval ());
      b.translate_axis (ext[LEFT] - refpoint->relative_coordinate (common, a),
                        a);
      return b;
    }
}
