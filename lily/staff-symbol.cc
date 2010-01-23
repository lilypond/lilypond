/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

	  span_points[d] = x->relative_coordinate (common, X_AXIS);
	  if (!x->break_status_dir ()
	      && !x->extent (x, X_AXIS).is_empty ())
	    span_points[d] += x->extent (x, X_AXIS)[d];
	}

      span_points[d] -= d* t / 2;
    }
  while (flip (&d) != LEFT);

  Stencil m;

  SCM line_positions = me->get_property ("line-positions");
  Stencil line
    = Lookup::horizontal_line (span_points
			       -me->relative_coordinate (common, X_AXIS),
			       t);

  Real space = staff_space (me);
  if (scm_is_pair (line_positions))
    {
      for (SCM s = line_positions; scm_is_pair (s);
	   s = scm_cdr (s))
	{
	  Stencil b (line);
	  b.translate_axis (scm_to_double (scm_car (s))
			    * 0.5 * space, Y_AXIS);
	  m.add_stencil (b);
	}
    }
  else
    {
      int l = Staff_symbol::line_count (me);
      Real height = (l - 1) * staff_space (me) / 2;
      for (int i = 0; i < l; i++)
	{
	  Stencil b (line);
	  b.translate_axis (height - i * space, Y_AXIS);
	  m.add_stencil (b);
	}
    }
  return m.smobbed_copy ();
}


int
Staff_symbol::get_steps (Grob *me)
{
  return line_count (me) * 2;
}

int
Staff_symbol::line_count (Grob *me)
{
  SCM c = me->get_property ("line-count");
  if (scm_is_number (c))
    return scm_to_int (c);
  else
    return 0;
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

MAKE_SCHEME_CALLBACK (Staff_symbol, height,1);
SCM
Staff_symbol::height  (SCM smob)
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
  y_ext.widen (t/2);
  return ly_interval2scm (y_ext);
}

bool
Staff_symbol::on_line (Grob *me, int pos)
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
      if (pos < min_line)
	return (( (int) (rint (pos - min_line)) % 2) == 0);
      if (pos > max_line)
	return (( (int) (rint (pos - max_line)) % 2) == 0);

      return false;
    }
  else
    return ((abs (pos + line_count (me)) % 2) == 1);
}

ADD_INTERFACE (Staff_symbol,
	       "This spanner draws the lines of a staff.  A staff symbol"
	       " defines a vertical unit, the @emph{staff space}.  Quantities"
	       " that go by a half staff space are called @emph{positions}."
	       "  The center (i.e., middle line or space) is position@tie{}0."
	       " The length of the symbol may be set by hand through the"
	       " @code{width} property.",

	       /* properties */
	       "ledger-line-thickness "
	       "line-count "
	       "line-positions "
	       "staff-space "
	       "thickness "
	       "width "
	       );
