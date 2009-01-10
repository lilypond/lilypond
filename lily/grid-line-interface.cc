/*
  grid-line-interface.cc --  implement Grid_line_interface

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "grid-line-interface.hh"

#include "grob.hh"
#include "pointer-group-interface.hh"
#include "lookup.hh"
#include "output-def.hh"

MAKE_SCHEME_CALLBACK (Grid_line_interface, print, 1);
SCM
Grid_line_interface::print (SCM smobbed_me)
{
  Grob *me = unsmob_grob (smobbed_me);

  extract_grob_set (me, "elements", elts);
  /* compute common refpoint of elements */
  Grob *refp = common_refpoint_of_array (elts, me, Y_AXIS);
  Interval iv;

  for (vsize i = 0; i < elts.size (); i++)
    {
      Grob *point = elts[i];

      iv.unite (point->extent (refp, Y_AXIS));
    }

  if (iv.is_empty ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real staffline = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real thick = robust_scm2double (me->get_property ("thickness"), 1.0)
    * staffline;

  iv += -me->relative_coordinate (refp, Y_AXIS);
  Stencil st = Lookup::filled_box (Box (Interval (0, thick),
					iv));

  return st.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Grid_line_interface, width, 1);
SCM
Grid_line_interface::width (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Real staffline = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real thick = robust_scm2double (me->get_property ("thickness"), 1.0)
    * staffline;

  return ly_interval2scm (Interval (0, thick));
}

void
Grid_line_interface::add_grid_point (Grob *me, Grob *b)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("elements"), b);
}

ADD_INTERFACE (Grid_line_interface,
	       "A line that is spanned between grid-points.",

	       /* properties */
	       "elements "
	       "thickness "
	       );

ADD_INTERFACE (Grid_point_interface,
	       "A spanning point for grid lines.",

	       /* properties */
	       ""
	       );
