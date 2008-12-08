/*
  measure-grouping-spanner.cc -- implement Measure_grouping

  source file of the GNU LilyPond music typesetter

  (c) 2002--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "measure-grouping-spanner.hh"

#include "output-def.hh"
#include "spanner.hh"
#include "lookup.hh"
#include "item.hh"
#include "staff-symbol-referencer.hh"

MAKE_SCHEME_CALLBACK (Measure_grouping, print, 1);
SCM
Measure_grouping::print (SCM grob)
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (grob));

  SCM which = me->get_property ("style");
  Real height = robust_scm2double (me->get_property ("height"), 1);

  Real t = Staff_symbol_referencer::line_thickness (me) * robust_scm2double (me->get_property ("thickness"), 1);
  Grob *common = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT),
							X_AXIS);

  Real right_point = robust_relative_extent (me->get_bound (RIGHT),
					     common, X_AXIS).linear_combination (CENTER);
  Real left_point = me->get_bound (LEFT)->relative_coordinate (common, X_AXIS);

  Interval iv (left_point, right_point);
  Stencil m;

  /*
    TODO: use line interface
  */
  if (which == ly_symbol2scm ("bracket"))
    m = Lookup::bracket (X_AXIS, iv, t, -height, t);
  else if (which == ly_symbol2scm ("triangle"))
    m = Lookup::triangle (iv, t, height);

  m.align_to (Y_AXIS, DOWN);
  m.translate_axis (- me->relative_coordinate (common, X_AXIS), X_AXIS);
  return m.smobbed_copy ();
}

ADD_INTERFACE (Measure_grouping,
	       "This object indicates groups of beats.  Valid choices for"
	       " @code{style} are @code{bracket} and @code{triangle}.",

	       /* properties */
	       "thickness "
	       "style "
	       "height "
	       );

