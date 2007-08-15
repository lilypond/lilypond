/*
  line-interface.cc -- implement Line_interface

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "line-interface.hh"

#include "staff-symbol-referencer.hh"
#include "lookup.hh"
#include "output-def.hh"

Stencil
Line_interface::make_arrow (Offset begin, Offset end,
			    Real thick,
			    Real length, Real width)
{
  Real angle = (end - begin).arg ();
  vector<Offset> points;

  points.push_back (Offset (0, 0));
  points.push_back (Offset (-length, width));
  points.push_back (Offset (-length, -width));

  for (vsize i = 0; i < points.size (); i++)
    points[i] = points[i] * complex_exp (Offset (0, angle)) + end;

  return Lookup::round_filled_polygon (points, thick);
}

Stencil
Line_interface::make_dashed_line (Real thick, Offset from, Offset to,
				  Real dash_period, Real dash_fraction)
{
  dash_fraction = min (max (dash_fraction, 0.0), 1.0);
  Real on = dash_fraction * dash_period + thick;
  Real off = dash_period - on;

  SCM at = scm_list_n (ly_symbol2scm ("dashed-line"),
		       scm_from_double (thick),
		       scm_from_double (on),
		       scm_from_double (off),
		       scm_from_double (to[X_AXIS] - from[X_AXIS]),
		       scm_from_double (to[Y_AXIS] - from[Y_AXIS]),
		       scm_from_double (0.0),
		       SCM_UNDEFINED);

  Box box;
  box.add_point (Offset (0, 0));
  box.add_point (to - from);

  box[X_AXIS].widen (thick / 2);
  box[Y_AXIS].widen (thick / 2);

  Stencil m = Stencil (box, at);
  m.translate (from);
  return m;
}

Stencil
Line_interface::make_line (Real th, Offset from, Offset to)
{
  SCM at = scm_list_n (ly_symbol2scm ("draw-line"),
		       scm_from_double (th),
		       scm_from_double (from[X_AXIS]),
		       scm_from_double (from[Y_AXIS]),
		       scm_from_double (to[X_AXIS]),
		       scm_from_double (to[Y_AXIS]),
		       SCM_UNDEFINED);

  Box box;
  box.add_point (from);
  box.add_point (to);

  box[X_AXIS].widen (th / 2);
  box[Y_AXIS].widen (th / 2);

  return Stencil (box, at);
}

Stencil
Line_interface::arrows (Grob *me, Offset from, Offset to,
			bool from_arrow,
			bool to_arrow)
{
  Stencil a;
  if (from_arrow || to_arrow)
    {
      Real thick = Staff_symbol_referencer::line_thickness (me)
	* robust_scm2double (me->get_property ("thickness"), 1);
      Real ss = Staff_symbol_referencer::staff_space (me);

      Real len = robust_scm2double (me->get_property ("arrow-length"), 1.3 * ss);
      Real wid = robust_scm2double (me->get_property ("arrow-width"), 0.5 * ss);

      if (to_arrow)
	a.add_stencil (make_arrow (from, to, thick, len, wid));

      if (from_arrow)
	a.add_stencil (make_arrow (to, from, thick, len, wid));
    }

  return a;
}

Stencil
Line_interface::line (Grob *me, Offset from, Offset to)
{
  Real thick = Staff_symbol_referencer::line_thickness (me)
    * robust_scm2double (me->get_property ("thickness"), 1);

  SCM type = me->get_property ("style");

  Stencil stil;

  SCM dash_fraction = me->get_property ("dash-fraction");
  if (scm_is_number (dash_fraction) || type == ly_symbol2scm ("dotted-line"))
    {

      Real fraction
	= type == ly_symbol2scm ("dotted-line")
	? 0.0
	: robust_scm2double (dash_fraction, 0.4);

      fraction = min (max (fraction, 0.0), 1.0);
      Real period = Staff_symbol_referencer::staff_space (me)
	* robust_scm2double (me->get_property ("dash-period"), 1.0);

      if (period <= 0)
	return Stencil ();

      Real len = (to-from).length();
      
      int n = (int) rint ((len - period * fraction) / period);
      if (n > 0)
	{
	  /*
	    TODO: figure out something intelligent for really short
	    sections.
	   */
	  period = ((to-from).length() - period * fraction) / n;
	}
      stil = make_dashed_line (thick, from, to, period, fraction);
    }
  else
    stil = make_line (thick, from, to);

  return stil;
}

ADD_INTERFACE (Line_interface, "line-interface",
	       "Generic line objects. Any object using lines supports this.  Normally, "
	       "you get a straight line. If @code{dash-period} is defined, a dashed line is "
	       "produced; the length of the dashes is tuned with "
	       "@code{dash-fraction}. If the latter is set to 0, a dotted line is "
	       "produced. If @code{dash-fraction} is negative, the line is made "
	       "transparent.",

	       /* properties */
	       "dash-period "
	       "dash-fraction "
	       "thickness "
	       "style "
	       "arrow-length "
	       "arrow-width")
