#include "self-alignment-interface.hh"
#include "warn.hh"

/*
  Position centered on parent.
 */
MAKE_SCHEME_CALLBACK (Self_alignment_interface,centered_on_parent,2);
SCM
Self_alignment_interface::centered_on_parent (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  Grob *him = me->get_parent (a);
  Interval he = him->extent (him,a);
  
  return  gh_double2scm (he.empty_b () ? 0.0 : he.center ());
}



/*
  Position centered on parent.
 */
MAKE_SCHEME_CALLBACK (Self_alignment_interface,centered_on_other_axis_parent,2);
SCM
Self_alignment_interface::centered_on_other_axis_parent (SCM element_smob,
							 SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  Grob *him = me->get_parent (other_axis (a));
  Interval he = him->extent (him,a);
  
  return  gh_double2scm (he.empty_b () ? 0.0 : he.center ());
}




/**
  callback that centers the element on itself

  Requires that self-alignment-{X,Y} be set.
 */
MAKE_SCHEME_CALLBACK (Self_alignment_interface,aligned_on_self,2);
SCM
Self_alignment_interface::aligned_on_self (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  static SCM  prop_syms[2];

  if (!prop_syms[0])
    {
      prop_syms[X_AXIS] = ly_symbol2scm ("self-alignment-X");
      prop_syms[Y_AXIS] = ly_symbol2scm ("self-alignment-Y");
    }
  
  SCM align (me->internal_get_grob_property (prop_syms[a]));
  if (gh_number_p (align))
    {
      Interval ext (me->extent (me,a));

      if (ext.empty_b ())
	{
	  programming_error ("I'm empty. Can't align on self");
	  return gh_double2scm (0.0);
	}
      else
	{
	  return gh_double2scm (- ext.linear_combination (gh_scm2double (align)));
	}
    }
  else if (unsmob_grob (align))
    {
      return gh_double2scm (- unsmob_grob (align)->relative_coordinate (me,  a));
    }
  return gh_double2scm (0.0);
}


ADD_INTERFACE (Self_alignment_interface, "self-alignment-interface",
  "Position self using some alignment",
  "self-alignment-X self-alignment-Y");

