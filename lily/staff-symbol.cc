/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "dimensions.hh"
#include "item.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"

using std::vector;

MAKE_SCHEME_CALLBACK (Staff_symbol, print, 1);

SCM
Staff_symbol::print (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
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

  for (LEFT_and_RIGHT (d))
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
          if (x->extent (x, X_AXIS).is_empty ()
              || (x->break_status_dir () && sp->broken_neighbor (d)))
            span_points[d] = x->relative_coordinate (common, X_AXIS);
          // What the default implementation of to-barline does for
          // spanners is not really in usefully recognizable shape by
          // now, so we just reimplement.
          else
            {
              SCM where = (d == RIGHT ? me->get_property ("break-align-symbols")
                                      : ly_symbol2scm ("break-alignment"));
              span_points[d] = Paper_column::break_align_width (x, where)[d];
            }
        }

      span_points[d] -= d * t / 2;
    }

  Stencil m;

  vector<Real> line_positions = Staff_symbol::line_positions (me);

  Stencil line = Lookup::horizontal_line (
      span_points - me->relative_coordinate (common, X_AXIS), t);

  Real space = staff_space (me);
  for (vector<Real>::const_iterator i = line_positions.begin (),
                                    e = line_positions.end ();
       i != e; ++i)
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
      return ly_scm2floatvector (line_positions);
    }
  else
    {
      int line_count = internal_line_count (me);
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
Staff_symbol::ledger_positions (Grob *me, int pos, Item const *head)
{
  // allow override of ledger positions via note head grob...
  if (head)
    {
      SCM posns = head->get_property ("ledger-positions");
      if (scm_is_pair (posns))
        return ly_scm2floatvector (posns);
    }

  // ...or via custom ledger positions function
  SCM lp_function = me->get_property ("ledger-positions-function");
  if (scm_is_pair (lp_function))
    {
      SCM func = scm_eval (lp_function, scm_interaction_environment ());
      if (ly_is_procedure (func))
        return ly_scm2floatvector (
            scm_call_2 (func, me->self_scm (), scm_from_int (pos)));
    }

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
       i != e; ++i)
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
    // custom ledger positions via StaffSymbol.ledger-positions
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
      int n = (int)floor ((ledger_fill[DOWN] - min_pos) / cycle);
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
      int ledger_count
          = (int)floor ((abs (nearest_line - pos) + ledger_extra) / 2);
      values.resize (ledger_count);
      for (int i = 0; i < ledger_count; i++)
        {
          values[i] = nearest_line + dir * (ledger_count - i) * 2;
        }
    }
  // remove any ledger lines that would fall on staff lines,
  // which can happen when ledger-extra > 0
  vector<Real> final_values;
  for (vector<Real>::const_iterator i = values.begin (), e = values.end ();
       i != e; ++i)
    {
      if (find (line_positions.begin (), line_positions.end (), *i)
          == line_positions.end ())
        {
          final_values.push_back (*i);
        }
    }
  return final_values;
}

// Get the line-count property directly.  This is for internal use when it is
// known that the line-positions property is not relevant.
int
Staff_symbol::internal_line_count (Grob *me)
{
  return robust_scm2int (me->get_property ("line-count"), 0);
}

Real
Staff_symbol::staff_space (Grob *me)
{
  Real ss = me->layout ()->get_dimension (ly_symbol2scm ("staff-space"));

  return robust_scm2double (me->get_property ("staff-space"), 1.0) * ss;
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
  Grob *me = unsmob<Grob> (smob);

  Interval y_ext = line_span (me); // units of staff position
  if (!y_ext.is_empty ())          // line count > 0
    {
      // convert staff position to height
      y_ext *= 0.5 * staff_space (me);

      // account for top and bottom line thickness
      Real t = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
      t *= robust_scm2double (me->get_property ("thickness"), 1.0);
      y_ext.widen (t / 2);
    }
  else
    {
      y_ext = Interval (0, 0);
    }

  return ly_interval2scm (y_ext);
}

bool
Staff_symbol::on_line (Grob *me, int pos, bool allow_ledger)
{
  // standard staff lines (any line count) and standard ledger lines
  if (!scm_is_pair (me->get_property ("line-positions"))
      && !scm_is_pair (me->get_property ("ledger-positions")))
    {
      int const line_cnt = internal_line_count (me);
      bool result = abs (pos + line_cnt) % 2 == 1;
      if (result && !allow_ledger)
        {
          result = -line_cnt < pos && pos < line_cnt;
        }
      return result;
    }

  // staff lines (custom or standard)
  vector<Real> lines = Staff_symbol::line_positions (me);
  for (vector<Real>::const_iterator i = lines.begin (), e = lines.end ();
       i != e; ++i)
    {
      if (pos == *i)
        return true;
    }

  // ledger lines (custom or standard)
  if (allow_ledger)
    {
      vector<Real> ledgers = Staff_symbol::ledger_positions (me, pos);
      if (ledgers.empty ())
        return false;
      for (vector<Real>::const_iterator i = ledgers.begin (),
                                        e = ledgers.end ();
           i != e; ++i)
        {
          if (pos == *i)
            return true;
        }
    }
  return false;
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
      // Note: This yields an empty interval (start > end) when there are no
      // staff lines.
      int count = internal_line_count (me);
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
               "break-align-symbols "
               "ledger-extra "
               "ledger-line-thickness "
               "ledger-positions "
               "ledger-positions-function "
               "line-count "
               "line-positions "
               "staff-space "
               "thickness "
               "width ");
