/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "staff-symbol.hh"

#include "lookup.hh"
#include "dimensions.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "warn.hh"
#include "item.hh"
#include "staff-symbol-referencer.hh"
#include "spanner.hh"

MAKE_SCHEME_CALLBACK (Staff_symbol, print, 1);

SCM
Staff_symbol::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner *sp = dynamic_cast<Spanner *> (me);
  Grob *common
    = sp->get_bound (LEFT)->common_refpoint (sp->get_bound (RIGHT), X_AXIS);

  Interval span_points (0, 0);

  /*
    For raggedright without ragged staves, simply set width to the linewidth.

    (ok -- lousy UI, since width is in staff spaces)

    --hwn.
  */
  Real t = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  t *= robust_scm2double (me->get_property ("thickness"), 1.0);

  Direction d = LEFT;
  do
    {
      SCM width_scm = me->get_property ("width");
      if (d == RIGHT && scm_is_number (width_scm))
        {
          /*
            don't multiply by Staff_symbol_referencer::staff_space (me),
            since that would make aligning staff symbols of different sizes to
            one right margin hell.
          */
          span_points[RIGHT] = scm_to_double (width_scm);
        }
      else
        {
          Item *x = sp->get_bound (d);

          span_points[d] = ((!x->break_status_dir ()
                             && !x->extent (x, X_AXIS).is_empty ())
                            ? Paper_column::break_align_width (x, ly_symbol2scm ("break-alignment"))[d]
                            : x->relative_coordinate (common, X_AXIS));
        }

      span_points[d] -= d * t / 2;
    }
  while (flip (&d) != LEFT);

  Stencil m;

  vector<Real> line_positions = Staff_symbol::line_positions (me);

  Stencil line
    = Lookup::horizontal_line (span_points
                               - me->relative_coordinate (common, X_AXIS),
                               t);

  Real space = staff_space (me);
  for (vector<Real>::const_iterator i = line_positions.begin (),
       e = line_positions.end ();
       i != e;
       ++i)
    {
      Stencil b (line);
      b.translate_axis (*i * 0.5 * space, Y_AXIS);
      m.add_stencil (b);
    }
  return m.smobbed_copy ();
}

vector<Real>
Staff_symbol::line_positions (Grob *me)
{
  SCM line_positions = me->get_property ("line-positions");
  if (scm_is_pair (line_positions))
    {
      int line_count = scm_ilength (line_positions);
      vector<Real> values (line_count);
      int i = 0;
      for (SCM s = line_positions; scm_is_pair (s);
           s = scm_cdr (s))
        {
          values[i++] = scm_to_double (scm_car (s));
        }
      return values;
    }
  else
    {
      int line_count = Staff_symbol::line_count (me);
      Real height = line_count - 1;
      vector<Real> values (line_count);
      for (int i = 0; i < line_count; i++)
        {
          values[i] = height - i * 2;
        }
      return values;
    }
}

vector<Real>
Staff_symbol::ledger_positions (Grob *me, int pos)
{
  SCM ledger_positions = me->get_property ("ledger-positions");
  Real ledger_extra = robust_scm2double (me->get_property ("ledger-extra"), 0);
  vector<Real> line_positions = Staff_symbol::line_positions (me);
  vector<Real> values;

  if (line_positions.empty ())
    return values;

  // find the staff line nearest to note position
  Real nearest_line = line_positions[0];
  Real line_dist = abs (line_positions[0] - pos);
  for (vector<Real>::const_iterator i = line_positions.begin (),
       e = line_positions.end ();
       i != e;
       ++i)
    {
      if (abs (*i - pos) < line_dist)
        {
          nearest_line = *i;
          line_dist = abs (*i - pos);
        }
    }

  if (line_dist < .5)
    return values;

  Direction dir = (Direction)sign (pos - nearest_line);

  if (scm_is_pair (ledger_positions))
    // custom ledger line positions
    {
      Real min_pos = HUGE_VAL;
      Real max_pos = -HUGE_VAL;
      SCM s2;

      // find the extent of the ledger pattern
      for (SCM s = ledger_positions; scm_is_pair (s); s = scm_cdr (s))
        {
          s2 = scm_car (s);
          if (!scm_is_number (s2))
            s2 = scm_car (s2);
          Real current_ledger = scm_to_double (s2);
          if (current_ledger > max_pos)
            max_pos = current_ledger;
          if (current_ledger < min_pos)
            min_pos = current_ledger;
        }

      Real cycle = max_pos - min_pos;

      Interval ledger_fill;
      ledger_fill.add_point (nearest_line + 0.5 * dir);
      ledger_fill.add_point (pos + 0.5 * dir + ledger_extra * dir);

      // fill the Interval ledger_fill with ledger lines
      int n = (int) floor ((ledger_fill[DOWN] - min_pos) / cycle);
      Real current;
      SCM s = scm_cdr (ledger_positions);
      if (!scm_is_pair (s) || cycle < 0.1)
        return values;
      do
        {
          s2 = scm_car (s);
          if (scm_is_number (s2))
            {
              current = scm_to_double (s2) + n * cycle;
              if (ledger_fill.contains (current))
                values.push_back (current);
            }
          else
            // grouped ledger lines, either add all or none
            {
              do
                {
                  current = scm_to_double (scm_car (s2)) + n * cycle;
                  if (ledger_fill.contains (current))
                    {
                      s2 = scm_car (s);
                      do
                        {
                          current = scm_to_double (scm_car (s2)) + n * cycle;
                          values.push_back (current);
                          s2 = scm_cdr (s2);
                        }
                      while (scm_is_pair (s2));
                    }
                  else
                    s2 = scm_cdr (s2);
                }
              while (scm_is_pair (s2));
            }
          s = scm_cdr (s);
          if (!scm_is_pair (s))
            {
              s = scm_cdr (ledger_positions);
              n++;
            }
        }
      while (current <= ledger_fill[UP]);
    }
  else
    // normal ledger lines
    {
      int ledger_count = (int) floor ((abs (nearest_line - pos) + ledger_extra) / 2);
      values.resize (ledger_count);
      for (int i = 0; i < ledger_count; i++)
        {
          values[i] = nearest_line + dir * (ledger_count - i) * 2;
        }
    }
  return values;
}

int
Staff_symbol::line_count (Grob *me)
{
  SCM line_positions = me->get_property ("line-positions");
  if (scm_is_pair (line_positions))
    return scm_ilength (line_positions);
  else
    return robust_scm2int (me->get_property ("line-count"), 0);
}

Real
Staff_symbol::staff_space (Grob *me)
{
  return robust_scm2double (me->get_property ("staff-space"), 1.0);
}

Real
Staff_symbol::get_line_thickness (Grob *me)
{
  Real lt = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));

  return robust_scm2double (me->get_property ("thickness"), 1.0) * lt;
}

Real
Staff_symbol::get_ledger_line_thickness (Grob *me)
{
  SCM lt_pair = me->get_property ("ledger-line-thickness");
  Offset z = robust_scm2offset (lt_pair, Offset (1.0, 0.1));

  return z[X_AXIS] * get_line_thickness (me) + z[Y_AXIS] * staff_space (me);
}

MAKE_SCHEME_CALLBACK (Staff_symbol, height, 1);
SCM
Staff_symbol::height (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Real t = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  t *= robust_scm2double (me->get_property ("thickness"), 1.0);

  SCM line_positions = me->get_property ("line-positions");

  Interval y_ext;
  Real space = staff_space (me);
  if (scm_is_pair (line_positions))
    {
      for (SCM s = line_positions; scm_is_pair (s);
           s = scm_cdr (s))
        y_ext.add_point (scm_to_double (scm_car (s)) * 0.5 * space);
    }
  else
    {
      int l = Staff_symbol::line_count (me);
      Real height = (l - 1) * staff_space (me) / 2;
      y_ext = Interval (-height, height);
    }
  y_ext.widen (t / 2);
  return ly_interval2scm (y_ext);
}

bool
Staff_symbol::on_line (Grob *me, int pos, bool allow_ledger)
{
  SCM line_positions = me->get_property ("line-positions");
  if (scm_is_pair (line_positions))
    {
      Real min_line = HUGE_VAL;
      Real max_line = -HUGE_VAL;
      for (SCM s = line_positions; scm_is_pair (s); s = scm_cdr (s))
        {
          Real current_line = scm_to_double (scm_car (s));
          if (pos == current_line)
            return true;
          if (current_line > max_line)
            max_line = current_line;
          if (current_line < min_line)
            min_line = current_line;

        }

      if (allow_ledger)
        {
          if (pos < min_line)
            return (( (int) (rint (pos - min_line)) % 2) == 0);
          if (pos > max_line)
            return (( (int) (rint (pos - max_line)) % 2) == 0);
        }

      return false;
    }
  else
    {
      int const line_cnt = line_count (me);
      bool result = abs (pos + line_cnt) % 2 == 1;
      if (result && !allow_ledger)
        {
          result = -line_cnt < pos && pos < line_cnt;
        }
      return result;
    }
}

Interval
Staff_symbol::line_span (Grob *me)
{
  SCM line_positions = me->get_property ("line-positions");
  Interval iv;

  if (scm_is_pair (line_positions))
    for (SCM s = line_positions; scm_is_pair (s); s = scm_cdr (s))
      iv.add_point (scm_to_double (scm_car (s)));
  else
    {
      int count = line_count (me);
      return Interval (-count + 1, count - 1);
    }

  return iv;
}

ADD_INTERFACE (Staff_symbol,
               "This spanner draws the lines of a staff.  A staff symbol"
               " defines a vertical unit, the @emph{staff space}.  Quantities"
               " that go by a half staff space are called @emph{positions}."
               "  The center (i.e., middle line or space) is position@tie{}0."
               " The length of the symbol may be set by hand through the"
               " @code{width} property.",

               /* properties */
               "ledger-extra "
               "ledger-line-thickness "
               "ledger-positions "
               "line-count "
               "line-positions "
               "staff-space "
               "thickness "
               "width "
              );
