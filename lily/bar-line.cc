/*
  bar-line.cc -- implement Bar_line

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "bar-line.hh"

#include "all-font-metrics.hh"
#include "font-interface.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "staff-symbol-referencer.hh"

MAKE_SCHEME_CALLBACK (Bar_line, print, 1);
SCM
Bar_line::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  SCM s = me->get_property ("glyph-name");
  SCM barsize = me->get_property ("bar-size");
  
  if (scm_is_string (s) && scm_is_number (barsize))
    {
      string str = ly_scm2string (s);
      Real sz = robust_scm2double (barsize, 0);
      if (sz <= 0)
	return SCM_EOL;

      return compound_barline (me, str, sz, false).smobbed_copy ();
    }
  return SCM_EOL;
}

Stencil
Bar_line::compound_barline (Grob *me, string str, Real h,
			    bool rounded)
{
  Real kern = robust_scm2double (me->get_property ("kern"), 1);
  Real thinkern = robust_scm2double (me->get_property ("thin-kern"), 1);
  Real hair = robust_scm2double (me->get_property ("hair-thickness"), 1);
  Real fatline = robust_scm2double (me->get_property ("thick-thickness"), 1);

  Real staffline = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  kern *= staffline;
  thinkern *= staffline;
  hair *= staffline;
  fatline *= staffline;

  Stencil thin = simple_barline (me, hair, h, rounded);
  Stencil thick = simple_barline (me, fatline, h, rounded);
  Stencil dot = Font_interface::get_default_font (me)->find_by_name ("dots.dot");

  int lines = Staff_symbol_referencer::line_count (me);
  Real dist
    = ((lines & 1 || lines == 0)
       ? 1
       : (staff_space < 2 ? 2 : .5)) * staff_space;
  Stencil colon (dot);
  colon.translate_axis (dist, Y_AXIS);
  colon.add_stencil (dot);
  colon.translate_axis (-dist / 2, Y_AXIS);

  Stencil m;
  if (str == "||:")
    str = "|:";

  if (str == "")
    return Lookup::blank (Box (Interval (0, 0), Interval (-h / 2, h / 2)));
  else if (str == "|")
    return thin;
  else if (str == "|." || (h == 0 && str == ":|"))
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0, 0);
      m.add_at_edge (X_AXIS, LEFT, thin, kern, 0);
    }
  else if (str == ".|" || (h == 0 && str == "|:"))
    {
      m.add_at_edge (X_AXIS, RIGHT, thick, 0, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern, 0);
    }
  else if (str == ":|")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0, 0);
      m.add_at_edge (X_AXIS, LEFT, thin, kern, 0);
      m.add_at_edge (X_AXIS, LEFT, colon, kern, 0);
    }
  else if (str == "|:")
    {
      m.add_at_edge (X_AXIS, RIGHT, thick, 0, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern, 0);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern, 0);
    }
  else if (str == ":|:")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, thinkern, 0);
      m.add_at_edge (X_AXIS, LEFT, colon, kern, 0);
      m.add_at_edge (X_AXIS, RIGHT, thick, kern, 0);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern, 0);
    }
  else if (str == ".|.")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, thinkern, 0);
      m.add_at_edge (X_AXIS, RIGHT, thick, kern, 0);
    }
  else if (str == "||")
    {
      /*
	should align to other side? this never appears
	on the system-start?
      */
      m.add_at_edge (X_AXIS, RIGHT, thin, 0, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, thinkern, 0);
    }
  else if (str == ":")
    {
      int c = (Staff_symbol_referencer::line_count (me));

      for (int i = 0; i < c - 1; i++)
	{
	  Real y = (- (c - 1.0) / 2 + 0.5 + i) * staff_space;
	  Stencil d (dot);

	  d.translate_axis (y, Y_AXIS);
	  m.add_stencil (d);
	}
    }
  else if (str == ".")
    {
      m = dot;
    }
  return m;
}

Stencil
Bar_line::simple_barline (Grob *me,
			  Real w,
			  Real h,
			  bool rounded)
{
  Real blot
    = rounded
    ? me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"))
    : 0.0;

  return Lookup::round_filled_box (Box (Interval (0, w),
					Interval (-h / 2, h / 2)), blot);
}

MAKE_SCHEME_CALLBACK (Bar_line, calc_bar_size, 1);
SCM
Bar_line::calc_bar_size (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (Grob *staff = Staff_symbol_referencer::get_staff_symbol (me))
    {
      Interval staff_y = staff->extent (staff, Y_AXIS);
      return scm_from_double (staff_y.is_empty () ? 0.0 : staff_y.length ());
    }
  return scm_from_int (0);
}


ADD_INTERFACE (Bar_line,
	       "bar-line-interface",

	       "Bar line.\n"
	       "\n"
	       "Print a special bar symbol. It replaces the \n"
	       "regular bar symbol with a special\n"
	       "symbol.  The argument @var{bartype} is a string which specifies the\n"
	       "kind of bar to print.  Options are @code{:|}, \n"
	       "@code{|:}, @code{:|:},\n"
	       "@code{||}, @code{|.},\n"
	       "@code{.|}, and @code{.|.}. \n"
	       "\n"
	       "These produce, respectively, a right repeat, a left repeat, a double\n"
	       "repeat, a double bar, a start bar, an end bar, and a thick double bar.\n"
	       "In addition, there is an option @code{||:} which is equivalent to\n"
	       "@code{|:} except at line breaks, where it produces a double bar (@code{||})\n"
	       "at the end of the line and a repeat sign (@code{|:}) at the beginning\n"
	       "of the new line."
	       "If @var{bartype} is set to @code{empty} then nothing is printed,\n"
	       "but a line break is allowed at that spot.\n",


	       /* properties */ 
	       "kern "
	       "thin-kern "
	       "hair-thickness "
	       "thick-thickness "
	       "glyph "
	       "glyph-name "
	       "bar-size "
	       );
