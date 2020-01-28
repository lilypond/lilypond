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

#include "lyric-hyphen.hh"

#include "axis-group-interface.hh"
#include "lookup.hh"
#include "moment.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "spanner.hh"

/*
  TODO: should extract hyphen dimensions or hyphen glyph from the
  font.
 */

MAKE_SCHEME_CALLBACK (Lyric_hyphen, print, 1);
SCM
Lyric_hyphen::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  Drul_array<Item *> bounds (me->get_bound (LEFT), me->get_bound (RIGHT));

  if (bounds[LEFT]->break_status_dir ()
      && (Paper_column::when_mom (bounds[LEFT])
          == Paper_column::when_mom (bounds[RIGHT]->get_column ())))
    return SCM_EOL;

  Grob *common = bounds[LEFT]->common_refpoint (bounds[RIGHT], X_AXIS);

  Interval span_points;
  for (LEFT_and_RIGHT (d))
    {
      Interval iv = Axis_group_interface::generic_bound_extent (bounds[d],
                                                                common, X_AXIS);

      span_points[d] = iv.is_empty ()
                           ? bounds[d]->relative_coordinate (common, X_AXIS)
                           : iv[-d];
    }

  Real lt = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real th = robust_scm2double (me->get_property ("thickness"), 1) * lt;
  Real font_size_step = robust_scm2double (me->get_property ("font-size"), 0.0);
  Real h = robust_scm2double (me->get_property ("height"), 0.5)
           * pow (2.0, font_size_step / 6.0);

  // interval?

  Real dash_period = robust_scm2double (me->get_property ("dash-period"), 1.0);
  Real dash_length = robust_scm2double (me->get_property ("length"), .5);
  Real padding = robust_scm2double (me->get_property ("padding"), 0.1);
  Real whiteout = robust_scm2double (me->get_property ("whiteout"), -1);

  if (dash_period < dash_length)
    dash_period = 1.5 * dash_length;

  Real l = span_points.length ();

  int n = int (ceil (l / dash_period - 0.5));
  if (n <= 0)
    n = 1;

  if (l < dash_length + 2 * padding && !bounds[RIGHT]->break_status_dir ())
    {
      Real minimum_length
          = robust_scm2double (me->get_property ("minimum-length"), .3);
      dash_length = std::max ((l - 2 * padding), minimum_length);
    }

  Real space_left = l - dash_length - (n - 1) * dash_period;

  /*
    If there is not enough space, the hyphen should disappear, but not
    at the end of the line.
  */
  if (space_left < 0.0 && !bounds[RIGHT]->break_status_dir ())
    return SCM_EOL;

  space_left = std::max (space_left, 0.0);

  Box b (Interval (0, dash_length), Interval (h, h + th));
  Stencil dash_mol (Lookup::round_filled_box (b, 0.8 * lt));

  Stencil total;
  for (int i = 0; i < n; i++)
    {
      Stencil m (dash_mol);
      m.translate_axis (span_points[LEFT] + i * dash_period + space_left / 2,
                        X_AXIS);
      total.add_stencil (m);
      if (whiteout > 0.0)
        {
          Box c (Interval (0, dash_length + 2 * whiteout * lt),
                 Interval (h - whiteout * lt, h + th + whiteout * lt));
          Stencil w (Lookup::round_filled_box (c, 0.8 * lt));
          w = w.in_color (1.0, 1.0, 1.0);
          w.translate_axis (span_points[LEFT] + i * dash_period + space_left / 2
                                - whiteout * lt,
                            X_AXIS);
          total.add_stencil (w);
        }
    }

  total.translate_axis (-me->relative_coordinate (common, X_AXIS), X_AXIS);
  return total.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Lyric_hyphen, set_spacing_rods, 1);
SCM
Lyric_hyphen::set_spacing_rods (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  Rod r;
  Spanner *sp = dynamic_cast<Spanner *> (me);

  r.distance_ = robust_scm2double (me->get_property ("minimum-distance"), 0);
  for (LEFT_and_RIGHT (d))
    {
      r.item_drul_[d] = sp->get_bound (d);
      if (r.item_drul_[d])
        r.distance_
            += -d * r.item_drul_[d]->extent (r.item_drul_[d], X_AXIS)[-d];
    }

  if (r.item_drul_[LEFT] && r.item_drul_[RIGHT])
    r.add_to_cols ();

  return SCM_UNSPECIFIED;
}

ADD_INTERFACE (Lyric_hyphen,
               "A centered hyphen is simply a line between lyrics used to"
               " divide syllables.",

               /* properties */
               "dash-period "
               "height "
               "length "
               "minimum-distance "
               "minimum-length "
               "padding "
               "thickness ");
