/*
  line-interface.cc -- implement Line_interface

  source file of the GNU LilyPond music typesetter

  (c) 2004--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "line-interface.hh"

#include "staff-symbol-referencer.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "grob.hh"
#include "font-interface.hh"

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
Line_interface::make_trill_line (Grob *me,
				 Offset from,
				 Offset to)
{
  Offset dz = (to-from);
  SCM alist_chain = Font_interface::text_font_alist_chain (me);
  SCM style_alist = scm_list_n (scm_cons (ly_symbol2scm ("font-encoding"),
					  ly_symbol2scm ("fetaMusic")),
				SCM_UNDEFINED);

  Font_metric *fm = select_font (me->layout (),
				 scm_cons (style_alist,
					   alist_chain));

  Stencil elt = fm->find_by_name ("scripts.trill_element");
  elt.align_to (Y_AXIS, CENTER);
  Real elt_len = elt.extent (X_AXIS).length ();
  if (elt_len <= 0.0)
    {
      programming_error ("can't find scripts.trill_element");
      return Stencil ();
    }
      
  Stencil line;
  Real len = 0.0;
  do
    {
      line.add_at_edge (X_AXIS, RIGHT, elt, 0);
      len = line.extent (X_AXIS).length ();
    }
  while (len + elt_len < dz.length ());

  line.rotate (dz.arg (), Offset (LEFT, CENTER));
  line.translate (from);

  return line; 
}


Stencil
Line_interface::make_zigzag_line (Grob *me,
				  Offset from,
				  Offset to)
{
  Offset dz = to -from;

  Real thick = Staff_symbol_referencer::line_thickness (me);
  thick *= robust_scm2double (me->get_property ("thickness"), 1.0); // todo: staff sym referencer? 

  Real staff_space = Staff_symbol_referencer::staff_space (me);

  Real w = robust_scm2double (me->get_property ("zigzag-width"), 1) * staff_space;
  int count = (int) ceil (dz.length () / w);
  w = dz.length () / count;

  Real l = robust_scm2double (me->get_property ("zigzag-length"), 1) * w;
  Real h = l > w / 2 ? sqrt (l * l - w * w / 4) : 0;

  Offset rotation_factor = complex_exp (Offset (0, dz.arg ()));

  Offset points[3];
  points[0] = Offset (0, -h / 2);
  points[1] = Offset (w / 2, h / 2);
  points[2] = Offset (w, -h / 2);
  for (int i = 0; i < 3; i++)
    points[i] = complex_multiply (points[i], rotation_factor);

  Stencil squiggle (Line_interface::make_line (thick, points[0], points[1]));
  squiggle.add_stencil (Line_interface::make_line (thick, points[1], points[2]));

  Stencil total;
  for (int i = 0; i < count; i++)
    {
      Stencil moved_squiggle (squiggle);
      moved_squiggle.translate (from + Offset (i * w, 0) * rotation_factor);
      total.add_stencil (moved_squiggle);
    }

  return total;
}


Stencil
Line_interface::make_dashed_line (Real thick, Offset from, Offset to,
				  Real dash_period, Real dash_fraction)
{
  dash_fraction = min (max (dash_fraction, 0.0), 1.0);
  Real on = dash_fraction * dash_period + thick;
  Real off = max (0.0, dash_period - on);

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
  if (type == ly_symbol2scm ("zigzag"))
    {
      return make_zigzag_line (me, from, to);
    }
  else if (type == ly_symbol2scm ("trill"))
    return make_trill_line (me, from, to);
  
  Stencil stencil;

  if (type == ly_symbol2scm ("dashed-line") || type == ly_symbol2scm ("dotted-line"))
    {

      Real fraction
	= type == ly_symbol2scm ("dotted-line")
	? 0.0
	: robust_scm2double (me->get_property ("dash-fraction"), 0.4);

      fraction = min (max (fraction, 0.0), 1.0);
      Real period = Staff_symbol_referencer::staff_space (me)
	* robust_scm2double (me->get_property ("dash-period"), 1.0);

      if (period <= 0)
	return Stencil ();

      Real len = (to-from).length ();
      
      int n = (int) rint ((len - period * fraction) / period);
      n = max (0, n);
      if (n > 0)
	{
	  /*
	    TODO: figure out something intelligent for really short
	    sections.
	   */
	  period = ((to-from).length () - period * fraction) / n;
	}
      stencil = make_dashed_line (thick, from, to, period, fraction);
    }
  else
    stencil = make_line (thick, from, to);

  return stencil;
}

ADD_INTERFACE (Line_interface,
	       "Generic line objects.  Any object using lines supports this."
	       "  The property @code{style} can be @code{line},"
	       " @code{dashed-line}, @code{trill}, @code{dotted-line} or"
	       " @code{zigzag}.\n"
	       "\n"
	       "For @code{dashed-line}, the length of the dashes is tuned"
	       " with @code{dash-fraction}.  If the latter is set to@tie{}0, a"
	       " dotted line is produced.  If @code{dash-period} is negative,"
	       " the line is made transparent.",

	       /* properties */
	       "dash-period "
	       "dash-fraction "
	       "thickness "
	       "style "
	       "zigzag-length "
	       "zigzag-width "
	       "arrow-length "
	       "arrow-width ")

