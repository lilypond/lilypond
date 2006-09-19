/*
  axis-group-interface.cc -- implement Axis_group_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "axis-group-interface.hh"

#include "pointer-group-interface.hh"
#include "grob.hh"
#include "hara-kiri-group-spanner.hh"
#include "warn.hh"

void
Axis_group_interface::add_element (Grob *me, Grob *e)
{
  SCM axes = me->get_property ("axes");
  if (!scm_is_pair (axes))
    programming_error ("axes should be nonempty");

  for (SCM ax = axes; scm_is_pair (ax); ax = scm_cdr (ax))
    {
      Axis a = (Axis) scm_to_int (scm_car (ax));

      if (!e->get_parent (a))
	e->set_parent (me, a);

      e->internal_set_object ((a == X_AXIS)
			      ? ly_symbol2scm ("axis-group-parent-X")
			      : ly_symbol2scm ("axis-group-parent-Y"),
			      me->self_scm ());
    }

  /* must be ordered, because Align_interface also uses
     Axis_group_interface  */
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("elements"), e);
}

bool
Axis_group_interface::has_axis (Grob *me, Axis a)
{
  SCM axes = me->get_property ("axes");

  return (SCM_BOOL_F != scm_memq (scm_from_int (a), axes));
}

Interval
Axis_group_interface::relative_group_extent (vector<Grob*> const &elts,
					     Grob *common, Axis a)
{
  Interval r;
  for (vsize i = 0; i < elts.size (); i++)
    {
      Grob *se = elts[i];
      Interval dims = se->extent (common, a);
      if (!dims.is_empty ())
	r.unite (dims);
    }
  return r;
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, width, 1);
SCM
Axis_group_interface::width (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return generic_group_extent (me, X_AXIS);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, height, 1);
SCM
Axis_group_interface::height (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return generic_group_extent (me, Y_AXIS);
}
  
SCM
Axis_group_interface::generic_group_extent (Grob *me, Axis a)
{
  extract_grob_set (me, "elements", elts);
  Grob *common = common_refpoint_of_array (elts, me, a);

  Real my_coord = me->relative_coordinate (common, a);
  Interval r (relative_group_extent (elts, common, a));

  return ly_interval2scm (r - my_coord);
}

void
Axis_group_interface::get_children (Grob *me, vector<Grob*> *found)
{
  found->push_back (me);

  if (!has_interface (me))
    return;

  extract_grob_set (me, "elements", elements);
  for (vsize i = 0; i < elements.size (); i++)
    {
      Grob *e = elements[i];
      Axis_group_interface::get_children (e, found);
    }
}

ADD_INTERFACE (Axis_group_interface, "axis-group-interface",

	       "An object that groups other layout objects.",

	       /* properties */
	       "axes "
	       "elements ");
