/*
  staff-symbol.cc -- implement Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
    For raggedright without ragged staffs, simply set width to the linewidth.

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

MAKE_SCHEME_CALLBACK(Staff_symbol,height,1);
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




ADD_INTERFACE (Staff_symbol, "staff-symbol-interface",
	       "This spanner draws the lines of a staff. "
	       "A staff symbol definines a vertical unit, the staff space. "
	       "Quantities that go by a half staff space are called positions "
	       "The center (i.e. middle line "
	       "or space) is position 0. The length of the symbol may be set by hand "
	       "through the @code{width} property. ",


	       /* properties */
	       "ledger-line-thickness "
	       "line-count "
	       "line-positions "
	       "staff-space "
	       "thickness "
	       "width "
	       );
