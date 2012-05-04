/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2012 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "duration.hh"
#include "font-interface.hh"
#include "international.hh"
#include "lookup.hh"
#include "misc.hh"
#include "moment.hh"
#include "output-def.hh"
#include "paper-column.hh" // urg
#include "percent-repeat-item.hh"
#include "rest.hh"
#include "separation-item.hh"
#include "spacing-options.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "system.hh"
#include "text-interface.hh"
#include "warn.hh"

Interval
Multi_measure_rest::bar_width (Spanner *me)
{
  SCM spacing_pair = me->get_property ("spacing-pair");
  Interval iv;
  for (LEFT_and_RIGHT (d))
    {
      Item *col = me->get_bound (d)->get_column ();
      SCM align_sym
        = (scm_is_pair (spacing_pair)
           ? index_get_cell (spacing_pair, d)
           : ly_symbol2scm ("staff-bar"));
      Interval coldim = Paper_column::break_align_width (col, align_sym);

      iv[d] = coldim[-d];
    }

  return iv;
}

MAKE_SCHEME_CALLBACK (Multi_measure_rest, percent, 1);
SCM
Multi_measure_rest::percent (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner *sp = dynamic_cast<Spanner *> (me);

  Stencil r = Percent_repeat_item_interface::x_percent (me, 1);
  r.translate_axis (-r.extent (X_AXIS).center (), X_AXIS);

  // ugh copy & paste.

  Grob *common_x = sp->get_bound (LEFT)->common_refpoint (sp->get_bound (RIGHT),
                                                          X_AXIS);
  Interval sp_iv = bar_width (sp);
  Real x_off = 0.0;

  Real rx = sp->get_bound (LEFT)->relative_coordinate (common_x, X_AXIS);
  /*
    we gotta stay clear of sp_iv, so move a bit to the right if
    needed.
  */
  x_off += max (sp_iv[LEFT] - rx, 0.0);

  /*
    center between stuff.
  */
  x_off += sp_iv.length () / 2;

  r.translate_axis (x_off, X_AXIS);

  return r.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Multi_measure_rest, print, 1);
SCM
Multi_measure_rest::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner *sp = dynamic_cast<Spanner *> (me);

  Interval sp_iv = bar_width (sp);
  Real space = sp_iv.length ();

  Real rx = sp->get_bound (LEFT)->relative_coordinate (0, X_AXIS);
  /*
    we gotta stay clear of sp_iv, so move a bit to the right if
    needed.
  */
  Real x_off = max (sp_iv[LEFT] - rx, 0.0);

  Stencil mol;
  mol.add_stencil (symbol_stencil (me, space));

  mol.translate_axis (x_off, X_AXIS);
  return mol.smobbed_copy ();
}

int
calc_closest_duration_log (Grob *me, double duration, bool force_round_up, bool paranoid)
{
  bool round_up = force_round_up
                  || to_boolean (me->get_property ("round-up-to-longer-rest"));
  int closest_usable_duration_log;

  // Out of range initial values.
  if (round_up)
    closest_usable_duration_log = -15; // high value
  else
    closest_usable_duration_log = 15; // low value
  int minimum_usable_duration_log = -15;
  int maximum_usable_duration_log = 15;

  SCM duration_logs_list = me->get_property ("usable-duration-logs");
  if (paranoid
      && (to_boolean (scm_null_p (duration_logs_list))
          || !to_boolean (scm_list_p (duration_logs_list))))
    {
      warning (_ ("usable-duration-logs must be a non-empty list.  Falling back to whole rests."));
      closest_usable_duration_log = 0;
    }
  else
    {
      for (SCM s = duration_logs_list; scm_is_pair (s); s = scm_cdr (s))
        {
          int dur_log = scm_to_int (scm_car (s));
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

int
calc_measure_duration_log (Grob *me, bool paranoid)
{
  SCM sml = dynamic_cast<Spanner *> (me)->get_bound (LEFT)
            ->get_property ("measure-length");
  Rational ml = (unsmob_moment (sml)) ? unsmob_moment (sml)->main_part_
                : Rational (1);
  double measure_duration = ml.Rational::to_double ();
  bool force_round_up = to_boolean (scm_list_p (scm_member (scm_cons (scm_from_int64 (ml.numerator ()),
                                                            scm_from_int64 (ml.denominator ())),
                                                            me->get_property ("round-up-exceptions"))));

  return calc_closest_duration_log (me, measure_duration, force_round_up, paranoid);
}

Stencil
Multi_measure_rest::symbol_stencil (Grob *me, Real space)
{
  int measure_count = 0;
  SCM m (me->get_property ("measure-count"));
  if (scm_is_number (m))
    measure_count = scm_to_int (m);
  if (measure_count <= 0)
    return Stencil ();

  SCM limit = me->get_property ("expand-limit");
  if (measure_count > scm_to_int (limit))
    {
      Real padding = 0.15;
      Stencil s = big_rest (me, (1.0 - 2 * padding) * space);
      s.translate_axis (padding * space, X_AXIS);
      return s;
    }

  Font_metric *musfont = Font_interface::get_default_font (me);
  int mdl = calc_measure_duration_log (me, true);

  if (measure_count == 1)
    {
      if (mdl == 0 && me->get_property ("staff-position") == SCM_EOL)
        {
          if (Staff_symbol_referencer::on_staff_line (me, 2))
            me->set_property ("staff-position", scm_from_int (2));
          else if (Staff_symbol_referencer::on_staff_line (me, 3))
            me->set_property ("staff-position", scm_from_int (3));
        }

      Stencil s = musfont->find_by_name (Rest::glyph_name (me, mdl, "", true));

      s.translate_axis ((space - s.extent (X_AXIS).length ()) / 2, X_AXIS);
      return s;
    }
  else
    return church_rest (me, musfont, measure_count, space);
}

/*
  WIDTH can also be 0 to determine the minimum size of the object.
*/
Stencil
Multi_measure_rest::big_rest (Grob *me, Real width)
{
  Real thick_thick = robust_scm2double (me->get_property ("thick-thickness"), 1.0);
  Real hair_thick = robust_scm2double (me->get_property ("hair-thickness"), .1);

  Real ss = Staff_symbol_referencer::staff_space (me);
  Real slt = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real y = slt * thick_thick / 2 * ss;
  Real ythick = hair_thick * slt * ss;
  Box b (Interval (0.0, max (0.0, (width - 2 * ythick))), Interval (-y, y));

  Real blot = width ? (.8 * min (y, ythick)) : 0.0;

  Stencil m = Lookup::round_filled_box (b, blot);
  Stencil yb = Lookup::round_filled_box (Box (Interval (-0.5, 0.5) * ythick, Interval (-ss, ss)), blot);

  m.add_at_edge (X_AXIS, RIGHT, yb, 0);
  m.add_at_edge (X_AXIS, LEFT, yb, 0);

  m.align_to (X_AXIS, LEFT);

  return m;
}

/*
  Kirchenpause (?)
*/
Stencil
Multi_measure_rest::church_rest (Grob *me, Font_metric *musfont, int measure_count,
                                 Real space)
{
  SCM mols = SCM_EOL;
  int symbol_count = 0;
  Real symbols_width = 0.0;
  double total_duration = measure_count * pow (2.0, -calc_measure_duration_log (me, true));

  while (total_duration > 0)
    {
      int dl = calc_closest_duration_log (me, total_duration, false, true);
      double duration = pow (2.0, -dl);

      total_duration -= duration;

      Stencil r = musfont->find_by_name (Rest::glyph_name (me, dl, "", true));
      if (dl == 0)
        {
          Real staff_space = Staff_symbol_referencer::staff_space (me);
          r.translate_axis (staff_space, Y_AXIS);
        }
      symbols_width += r.extent (X_AXIS).length ();
      mols = scm_cons (r.smobbed_copy (), mols);
      symbol_count++;
    }

  /* Make outer padding this much bigger.  */
  Real outer_padding_factor = 1.5;
  Real inner_padding = (space - symbols_width)
                       / (2 * outer_padding_factor + (symbol_count - 1));
  if (inner_padding < 0)
    inner_padding = 1.0;

  Stencil mol;
  for (SCM s = mols; scm_is_pair (s); s = scm_cdr (s))
    mol.add_at_edge (X_AXIS, LEFT, *unsmob_stencil (scm_car (s)),
                     inner_padding);
  mol.align_to (X_AXIS, LEFT);
  mol.translate_axis (outer_padding_factor * inner_padding, X_AXIS);

  return mol;
}

void
Multi_measure_rest::add_column (Grob *me, Item *c)
{
  add_bound_item (dynamic_cast<Spanner *> (me), c);
}

void
Multi_measure_rest::calculate_spacing_rods (Grob *me, Real length)
{
  Spanner *sp = dynamic_cast<Spanner *> (me);
  if (! (sp->get_bound (LEFT) && sp->get_bound (RIGHT)))
    {
      programming_error ("Multi_measure_rest::get_rods (): I am not spanned!");
      return;
    }

  Item *li = sp->get_bound (LEFT)->get_column ();
  Item *ri = sp->get_bound (RIGHT)->get_column ();
  Item *lb = li->find_prebroken_piece (RIGHT);
  Item *rb = ri->find_prebroken_piece (LEFT);
  Grob *spacing = unsmob_grob (li->get_object ("spacing"));
  if (!spacing)
    spacing = unsmob_grob (ri->get_object ("spacing"));
  if (!spacing)
    me->warning ("Using naive multi measure rest spacing.");
  else
    {
      Spacing_options options;
      options.init_from_grob (me);
      int dl = calc_measure_duration_log (me, false);
      Duration dur = Duration (dl, 0);
      Rational rat = dur.get_length ();
      length = max (length, options.get_duration_space (rat));
    }

  Item *combinations[4][2] = {{li, ri},
    {lb, ri},
    {li, rb},
    {lb, rb}
  };

  for (int i = 0; i < 4; i++)
    {
      Item *li = combinations[i][0];
      Item *ri = combinations[i][1];

      if (!li || !ri)
        continue;

      Rod rod;
      rod.item_drul_[LEFT] = li;
      rod.item_drul_[RIGHT] = ri;

      rod.distance_ = Paper_column::minimum_distance (li, ri)
                      + length
                      + 2 * robust_scm2double (me->get_property ("bound-padding"), 1.0);

      Real minlen = robust_scm2double (me->get_property ("minimum-length"), 0);
      rod.distance_ = max (rod.distance_, minlen);
      rod.add_to_cols ();
    }
}

MAKE_SCHEME_CALLBACK (Multi_measure_rest, set_spacing_rods, 1);
SCM
Multi_measure_rest::set_spacing_rods (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Real sym_width = symbol_stencil (me, 0.0).extent (X_AXIS).length ();
  calculate_spacing_rods (me, sym_width);

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Multi_measure_rest, set_text_rods, 1);
SCM
Multi_measure_rest::set_text_rods (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Stencil *stil = me->get_stencil ();

  /* FIXME uncached */
  Real len = (stil && !stil->extent (X_AXIS).is_empty ())
             ? stil->extent (X_AXIS).length ()
             : 0.0;
  calculate_spacing_rods (me, len);

  return SCM_UNSPECIFIED;
}

ADD_INTERFACE (Multi_measure_rest,
               "A rest that spans a whole number of measures.",

               /* properties */
               "bound-padding "
               "expand-limit "
               "hair-thickness "
               "measure-count "
               "minimum-length "
               "round-up-exceptions "
               "round-up-to-longer-rest "
               "spacing-pair "
               "thick-thickness "
               "usable-duration-logs "
              );
