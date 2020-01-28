/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2003--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "axis-group-interface.hh"
#include "bracket.hh"
#include "item.hh"
#include "spanner.hh"
#include "stencil.hh"

struct Piano_pedal_bracket
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

MAKE_SCHEME_CALLBACK (Piano_pedal_bracket, print, 1);
SCM
Piano_pedal_bracket::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  Spanner *orig = me->original ();

  Drul_array<bool> broken (false, false);
  Drul_array<Real> height
      = robust_scm2drul (me->get_property ("edge-height"), Interval (0, 0));
  Drul_array<Real> shorten
      = robust_scm2drul (me->get_property ("shorten-pair"), Interval (0, 0));
  Drul_array<Real> flare
      = robust_scm2drul (me->get_property ("bracket-flare"), Interval (0, 0));

  Grob *common
      = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);
  Grob *textbit = unsmob<Grob> (me->get_object ("pedal-text"));

  if (textbit)
    common = common->common_refpoint (textbit, X_AXIS);

  Interval span_points (0, 0);
  for (LEFT_and_RIGHT (d))
    {
      Item *b = me->get_bound (d);
      broken[d] = b->break_status_dir () != CENTER;
      if (broken[d])
        {
          if (orig
              && ((d == RIGHT
                   && me->get_break_index () != orig->broken_intos_.size () - 1)
                  || (d == LEFT && me->get_break_index ())))
            height[d] = 0.0;
          else
            flare[d] = 0.0;

          span_points[d] = Axis_group_interface::generic_bound_extent (
              b, common, X_AXIS)[RIGHT];
        }
      else
        span_points[d] = b->relative_coordinate (common, X_AXIS);
    }

  /* For 'Mixed' style pedals, i.e.  a bracket preceded by text:  Ped._____|
     need to shorten by the extent of the text grob
  */
  if (textbit)
    {
      height[LEFT] = 0;

      Real padding = robust_scm2double (me->get_property ("bound-padding"), 0);

      span_points[LEFT]
          = padding + robust_relative_extent (textbit, common, X_AXIS)[RIGHT];
    }

  Stencil m;
  if (!span_points.is_empty () && span_points.length () > 0.001)
    {
      m = Bracket::make_bracket (me, Y_AXIS, Offset (span_points.length (), 0),
                                 height, Interval (), flare, shorten);
    }
  m.translate_axis (
      span_points[LEFT] - me->relative_coordinate (common, X_AXIS), X_AXIS);
  return m.smobbed_copy ();
}

ADD_INTERFACE (Piano_pedal_bracket,
               "The bracket of the piano pedal.  It can be tuned through"
               " the regular bracket properties.",

               /* properties */
               "bound-padding "
               "bracket-flare "
               "dashed-edge "
               "edge-height "
               "pedal-text "
               "shorten-pair ");
