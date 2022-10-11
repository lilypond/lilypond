/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "multi-measure-rest.hh"

#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "international.hh"
#include "lookup.hh"
#include "ly-scm-list.hh"
#include "ly-smob-list.hh"
#include "misc.hh"
#include "moment.hh"
#include "output-def.hh"
#include "paper-column.hh" // urg
#include "rest.hh"
#include "separation-item.hh"
#include "spacing-options.hh"
#include "spanner.hh"
#include "staff-symbol.hh"
#include "staff-symbol-referencer.hh"
#include "system.hh"
#include "text-interface.hh"
#include "warn.hh"

Interval
Multi_measure_rest::bar_width (Spanner *me)
{
  SCM spacing_pair = get_property (me, "spacing-pair");
  Interval iv;
  for (const auto d : {LEFT, RIGHT})
    {
      Item *col = me->get_bound (d)->get_column ();
      SCM align_sym
        = (scm_is_pair (spacing_pair) ? index_get_cell (spacing_pair, d)
                                      : ly_symbol2scm ("staff-bar"));
      Interval coldim = Paper_column::break_align_width (col, align_sym);

      iv[d] = coldim[-d];
    }

  return iv;
}

MAKE_SCHEME_CALLBACK (Multi_measure_rest, print, "ly:multi-measure-rest::print",
                      1);
SCM
Multi_measure_rest::print (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Spanner, smob, 1);

  Interval sp_iv = bar_width (me);
  Real space = sp_iv.length ();

  Real rx = me->get_bound (LEFT)->relative_coordinate (0, X_AXIS);
  /*
    we gotta stay clear of sp_iv, so move a bit to the right if
    needed.
  */
  Real x_off = std::max (sp_iv[LEFT] - rx, 0.0);

  Stencil mol;
  mol.add_stencil (symbol_stencil (me, space));

  mol.translate_axis (x_off, X_AXIS);
  return mol.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Multi_measure_rest, height,
                      "ly:multi-measure-rest::height", 1);
SCM
Multi_measure_rest::height (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Spanner, smob, 1);

  Real space = 1000000; // something very large...

  Stencil mol;
  mol.add_stencil (symbol_stencil (me, space));

  return to_scm (mol.extent (Y_AXIS));
}

int
calc_measure_duration_log (Spanner *me)
{
  SCM sml = get_property (me->get_bound (LEFT), "measure-length");
  Rational ml
    = (unsmob<Moment> (sml)) ? unsmob<Moment> (sml)->main_part_ : Rational (1);
  auto duration = static_cast<double> (ml);
  bool round_up
    = from_scm<bool> (scm_list_p (scm_member (
        scm_cons (to_scm (ml.numerator ()), to_scm (ml.denominator ())),
        get_property (me, "round-up-exceptions"))))
      || from_scm<bool> (get_property (me, "round-up-to-longer-rest"));
  int closest_usable_duration_log;

  // Out of range initial values.
  if (round_up)
    closest_usable_duration_log = -15; // high value
  else
    closest_usable_duration_log = 15; // low value
  int minimum_usable_duration_log = -15;
  int maximum_usable_duration_log = 15;

  SCM duration_logs_list = get_property (me, "usable-duration-logs");
  if (from_scm<bool> (scm_null_p (duration_logs_list))
      || !from_scm<bool> (scm_list_p (duration_logs_list)))
    {
      warning (_ ("usable-duration-logs must be a non-empty list."
                  "  Falling back to whole rests."));
      closest_usable_duration_log = 0;
    }
  else
    {
      for (const auto &dur_log : as_ly_scm_list_t<int> (duration_logs_list))
        {
          if (dur_log > minimum_usable_duration_log)
            minimum_usable_duration_log = dur_log;
          if (dur_log < maximum_usable_duration_log)
            maximum_usable_duration_log = dur_log;
          double dur = pow (2.0, -dur_log);
          if (round_up)
            {
              if (duration <= dur && dur_log > closest_usable_duration_log)
                closest_usable_duration_log = dur_log;
            }
          else
            {
              if (duration >= dur && dur_log < closest_usable_duration_log)
                closest_usable_duration_log = dur_log;
            }
        }
    }

  if (closest_usable_duration_log == 15)
    closest_usable_duration_log = minimum_usable_duration_log;
  if (closest_usable_duration_log == -15)
    closest_usable_duration_log = maximum_usable_duration_log;

  return closest_usable_duration_log;
}

Stencil
Multi_measure_rest::symbol_stencil (Spanner *me, Real space)
{
  int measure_count = 0;
  SCM m (get_property (me, "measure-count"));
  if (scm_is_number (m))
    measure_count = from_scm<int> (m);
  if (measure_count <= 0)
    return Stencil ();

  SCM limit = get_property (me, "expand-limit");
  if (measure_count > from_scm<int> (limit))
    {
      Real padding = 0.15;
      Stencil s = big_rest (me, (1.0 - 2 * padding) * space);
      s.translate_axis (padding * space, X_AXIS);
      return s;
    }
  else
    {
      Font_metric *musfont = Font_interface::get_default_font (me);
      int mdl = calc_measure_duration_log (me);
      return church_rest (me, musfont, measure_count, mdl, space);
    }
}

/*
  WIDTH can also be 0 to determine the minimum size of the object.
*/
Stencil
Multi_measure_rest::big_rest (Grob *me, Real width)
{
  Real thick_thick
    = from_scm<double> (get_property (me, "thick-thickness"), 1.0);
  Real hair_thick = from_scm<double> (get_property (me, "hair-thickness"), .1);

  Real ss = Staff_symbol_referencer::staff_space (me);
  Real slt = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real y = slt * thick_thick / 2 * ss;
  Real ythick = hair_thick * slt * ss;
  Box b (Interval (0.0, std::max (0.0, (width - 2 * ythick))),
         Interval (-y, y));

  Real blot = width ? (.8 * std::min (y, ythick)) : 0.0;

  Stencil m = Lookup::filled_box (b);
  Stencil yb = Lookup::round_filled_box (
    Box (Interval (-0.5, 0.5) * ythick, Interval (-ss, ss)), blot);

  m.add_at_edge (X_AXIS, RIGHT, yb, 0);
  m.add_at_edge (X_AXIS, LEFT, yb, 0);

  m.align_to (X_AXIS, LEFT);

  return m;
}

/*
  Kirchenpause (?)
*/
Stencil
Multi_measure_rest::church_rest (Grob *me, Font_metric *musfont,
                                 int measure_count, int mdl, Real space)
{
  // using double here is not less exact than rationals because
  // only simple, unscaled durations are used for representation
  // even if you have \time 10/6
  double displayed_duration = measure_count * pow (2.0, -mdl);
  SCM mols = SCM_EOL;
  int symbol_count = 0;
  Real symbols_width = 0.0;
  const auto dir = get_grob_direction (me);

  SCM sp = get_property (me, "staff-position");
  Real pos;

  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  // If there is no StaffSymbol, print MMrests on one (invisible) line.
  bool oneline = (!staff) || Staff_symbol::line_positions (staff).size () < 2;

  if (scm_is_null (sp))
    {
      if (1 <= displayed_duration
          && displayed_duration < 2) // i. e. longest rest symbol is semibreve
        {
          pos = Rest::staff_position_internal (me, 0, dir) - (oneline ? 0 : 2);
        }
      else
        pos = Rest::staff_position_internal (me, 1, dir);
      set_property (me, "staff-position", to_scm (pos));
    }
  else
    pos = from_scm<double> (sp);

  int dl = -3;
  while (displayed_duration > 0)
    {
      double duration = pow (2.0, -dl);

      if (displayed_duration < duration)
        {
          dl++;
          continue;
        }

      displayed_duration -= duration;

      Real ss = Staff_symbol_referencer::staff_space (me);
      Real spi = Rest::staff_position_internal (me, dl, dir);
      Stencil r;
      if (oneline && (dl == 0 || (dl < 0 && !dir)))
        {
          spi -= 2;
          r = musfont->find_by_name (
            Rest::glyph_name (me, dl, "", true, (dl == 0) ? 0 : -2));
        }
      else
        r = musfont->find_by_name (
          Rest::glyph_name (me, dl, "", true, (dl == 0) ? 2 : 0));
      if (dl < 0)
        {
          Real fs
            = pow (2, from_scm<double> (get_property (me, "font-size"), 0) / 6);
          r.translate_axis (ss * 0.5 * (spi - pos) + (ss - fs), Y_AXIS);
        }
      else
        r.translate_axis (ss * 0.5 * (spi - pos), Y_AXIS);

      symbols_width += r.extent (X_AXIS).length ();
      mols = scm_cons (r.smobbed_copy (), mols);
      symbol_count++;
    }

  /*
    When symbols spread to fullest extent, outer padding is this much
    bigger.
  */
  Real outer_padding_factor = 1.5;
  /* Widest gap between symbols; to be limited by max-symbol-separation */
  Real inner_padding
    = (space - symbols_width) / (2 * outer_padding_factor + (symbol_count - 1));
  if (inner_padding < 0)
    inner_padding = 1.0;

  Real max_separation = std::max (
    from_scm<double> (get_property (me, "max-symbol-separation"), 8.0), 1.0);

  inner_padding = std::min (inner_padding, max_separation);
  Real left_offset
    = (space - symbols_width - (inner_padding * (symbol_count - 1))) / 2;

  Stencil mol;
  for (auto *s : as_ly_smob_list<const Stencil> (mols))
    mol.add_at_edge (X_AXIS, LEFT, *s, inner_padding);
  mol.align_to (X_AXIS, LEFT);
  mol.translate_axis (left_offset, X_AXIS);

  return mol;
}

void
Multi_measure_rest::add_column (Spanner *me, Item *c)
{
  add_bound_item (me, c);
}

void
Multi_measure_rest::calculate_spacing_rods (Spanner *me, Real length)
{
  if (!(me->get_bound (LEFT) && me->get_bound (RIGHT)))
    {
      programming_error ("Multi measure rest seems misplaced.");
      return;
    }

  Item *li = me->get_bound (LEFT)->get_column ();
  Item *ri = me->get_bound (RIGHT)->get_column ();
  Item *lb = li->find_prebroken_piece (RIGHT);
  Item *rb = ri->find_prebroken_piece (LEFT);

  Grob *spacing = unsmob<Grob> (get_object (li, "spacing"));
  if (!spacing)
    spacing = unsmob<Grob> (get_object (ri, "spacing"));
  if (spacing)
    {
      Spacing_options options;
      options.init_from_grob (me);
      const auto mlen
        = from_scm (get_property (li, "measure-length"), Moment (1));
      length
        += from_scm<double> (get_property (li, "full-measure-extra-space"), 0.0)
           + options.get_duration_space (mlen.main_part_)
           + (from_scm<double> (get_property (me, "space-increment"), 0.0)
              * log_2 (from_scm (get_property (me, "measure-count"), 1)));
    }

  length += 2 * from_scm<double> (get_property (me, "bound-padding"), 0.0);

  Real minlen = from_scm<double> (get_property (me, "minimum-length"), 0.0);

  Item *combinations[4][2] = {{li, ri}, {lb, ri}, {li, rb}, {lb, rb}};

  for (int i = 0; i < 4; i++)
    {
      Item *li = combinations[i][0];
      Item *ri = combinations[i][1];

      if (!li || !ri)
        continue;

      Rod rod;
      rod.item_drul_[LEFT] = li;
      rod.item_drul_[RIGHT] = ri;

      rod.distance_
        = std::max (Paper_column::minimum_distance (li, ri) + length, minlen);
      rod.add_to_cols ();
    }
}

MAKE_SCHEME_CALLBACK (Multi_measure_rest, set_spacing_rods,
                      "ly:multi-measure-rest::set-spacing-rods", 1);
SCM
Multi_measure_rest::set_spacing_rods (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Spanner, smob, 1);
  Real sym_width = symbol_stencil (me, 0.0).extent (X_AXIS).length ();
  calculate_spacing_rods (me, sym_width);

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Multi_measure_rest, set_text_rods,
                      "ly:multi-measure-rest::set-text-rods", 1);
SCM
Multi_measure_rest::set_text_rods (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Spanner, smob, 1);
  auto *stil = me->get_stencil ();

  /* FIXME uncached */
  Real len = (stil && !stil->extent (X_AXIS).is_empty ())
               ? stil->extent (X_AXIS).length ()
               : 0.0;
  calculate_spacing_rods (me, len);

  return SCM_UNSPECIFIED;
}

ADD_INTERFACE (Multi_measure_rest,
               R"(
A rest that spans a whole number of measures.
               )",

               /* properties */
               R"(
bound-padding
expand-limit
hair-thickness
max-symbol-separation
measure-count
minimum-length
round-up-exceptions
round-up-to-longer-rest
space-increment
spacing-pair
thick-thickness
usable-duration-logs
               )");
