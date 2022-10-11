/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2003--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "output-def.hh"
#include "paper-column.hh"
#include "moment.hh"
#include "spanner.hh"
#include "system.hh"

/*
  TODO: should extract hyphen dimensions or hyphen glyph from the
  font.
 */

MAKE_SCHEME_CALLBACK (Lyric_hyphen, print, "ly:lyric-hyphen::print", 1);
SCM
Lyric_hyphen::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  const auto bounds = me->get_bounds ();

  // FIXME: does this bring anything more than
  // ly:spanner::kill-zero-spanned-time in
  // after-line-breaking? --JeanAS
  if (bounds[LEFT]->break_status_dir ()
      && (Paper_column::when_mom (bounds[LEFT])
            == Paper_column::when_mom (bounds[RIGHT]->get_column ())
          && !from_scm<bool> (get_property (me, "after-line-breaking"))))
    return SCM_EOL;

  Grob *common = bounds[LEFT]->common_refpoint (bounds[RIGHT], X_AXIS);

  Interval span_points;
  for (const auto d : {LEFT, RIGHT})
    {
      Interval iv = Axis_group_interface::generic_bound_extent (bounds[d],
                                                                common, X_AXIS);

      span_points[d] = iv.is_empty ()
                         ? bounds[d]->relative_coordinate (common, X_AXIS)
                         : iv[-d];
    }

  Real lt = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real th = from_scm<double> (get_property (me, "thickness"), 1) * lt;
  Real font_size_step = from_scm<double> (get_property (me, "font-size"), 0.0);
  Real h = from_scm<double> (get_property (me, "height"), 0.5)
           * pow (2.0, font_size_step / 6.0);

  // interval?

  Real dash_period = from_scm<double> (get_property (me, "dash-period"), 1.0);
  Real dash_length = from_scm<double> (get_property (me, "length"), .5);
  Real padding = from_scm<double> (get_property (me, "padding"), 0.1);
  Real whiteout = from_scm<double> (get_property (me, "whiteout"), -1);

  if (dash_period < dash_length)
    dash_period = 1.5 * dash_length;

  Real l = span_points.length ();

  int n = int (ceil (l / dash_period - 0.5));
  if (n <= 0)
    n = 1;

  if (l < dash_length + 2 * padding && !bounds[RIGHT]->break_status_dir ())
    {
      Real minimum_length
        = from_scm<double> (get_property (me, "minimum-length"), .3);
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

MAKE_SCHEME_CALLBACK (Lyric_hyphen, set_spacing_rods,
                      "ly:lyric-hyphen::set-spacing-rods", 1);
SCM
Lyric_hyphen::set_spacing_rods (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Spanner, smob, 1);

  System *root = get_root_system (me);
  const auto bounds = me->get_bounds ();
  if (!bounds[LEFT] || !bounds[RIGHT])
    return SCM_UNSPECIFIED;
  std::vector<Item *> cols (root->broken_col_range (
    bounds[LEFT]->get_column (), bounds[RIGHT]->get_column ()));

  Rod rod;
  rod.distance_ = from_scm<double> (get_property (me, "minimum-distance"), 0);
  rod.item_drul_ = bounds;
  rod.distance_ += rod.bounds_protrusion ();
  rod.add_to_cols ();

  if (cols.size ()
      && from_scm<bool> (get_property_data (me, "after-line-breaking")))
    {
      Rod rod_after_break;
      rod_after_break.item_drul_[LEFT]
        = cols.back ()->find_prebroken_piece (RIGHT);
      rod_after_break.item_drul_[RIGHT] = bounds[RIGHT];
      rod_after_break.distance_
        = from_scm<double> (get_property (me, "length"), 0.5);
      rod_after_break.distance_
        += from_scm<double> (get_property (me, "padding"), 0.1) * 2;
      rod_after_break.distance_ += rod_after_break.bounds_protrusion ();
      rod_after_break.add_to_cols ();
    }

  return SCM_UNSPECIFIED;
}

ADD_INTERFACE (Lyric_hyphen,
               R"(
A centered hyphen is simply a line between lyrics used to divide syllables.
               )",

               /* properties */
               R"(
dash-period
height
length
minimum-distance
minimum-length
padding
thickness
               )");
