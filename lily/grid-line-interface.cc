/*
  grid-line-interface.cc --  implement Grid_line_interface

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "group-interface.hh"
#include "output-def.hh"
#include "stencil.hh"
#include "lookup.hh"
#include "grid-line-interface.hh"
#include "grob.hh"


MAKE_SCHEME_CALLBACK (Grid_line_interface, print, 1);
SCM
Grid_line_interface::print (SCM smobbed_me)
{
  Grob *me = unsmob_grob (smobbed_me);
  SCM first_elt = me->get_property ("elements");

  /* compute common refpoint of elements */
  Grob *refp = common_refpoint_of_list (first_elt, me, Y_AXIS);
  Interval iv;
  
  for (SCM elts = first_elt; scm_is_pair (elts); elts = scm_cdr (elts))
    {
      Grob *point = unsmob_grob (scm_car (elts));

      iv.unite (point->extent (refp, Y_AXIS));
    }

  if (iv.is_empty ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real staffline = me->get_layout ()->get_dimension (ly_symbol2scm ("linethickness"));
  Real thick = robust_scm2double (me->get_property ("thickness"), 1.0)
    * staffline;


  iv += - me->relative_coordinate (refp, Y_AXIS);
  Stencil st = Lookup::filled_box (Box (Interval (0, thick),
					iv));

  return st.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Grid_line_interface, width_callback, 2);
SCM
Grid_line_interface::width_callback (SCM element_smob, SCM scm_axis)
{
  Grob *me = unsmob_grob (element_smob);
  (void) scm_axis;
  assert (scm_to_int (scm_axis) == X_AXIS);

  Real staffline = me->get_layout ()->get_dimension (ly_symbol2scm ("linethickness"));
  Real thick = robust_scm2double (me->get_property ("thickness"), 1.0)
    * staffline;
  
  return ly_interval2scm (Interval (0, thick));
}

void
Grid_line_interface::add_grid_point (Grob *me, Grob *b)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("elements"), b);
  me->add_dependency (b);
}

ADD_INTERFACE (Grid_line_interface, "grid-line-interface",
	       "A  line that spanned between grid-points. ",
	       "elements thickness");


ADD_INTERFACE (Grid_point_interface, "grid-point-interface",
	       "A spanning point for grid lines. ",
	       "");

