/*
  side-position-interface.cc -- implement Side_position_interface

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "side-position-interface.hh"

#include <cmath>		// ceil.
#include <algorithm>
using namespace std;

#include "note-head.hh"
#include "warn.hh"
#include "warn.hh"
#include "dimensions.hh"
#include "staff-symbol-referencer.hh"
#include "pointer-group-interface.hh"
#include "directional-element-interface.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "string-convert.hh"
#include "misc.hh"

void
Side_position_interface::add_support (Grob *me, Grob *e)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("side-support-elements"), e);
}

Direction
Side_position_interface::get_direction (Grob *me)
{
  Direction relative_dir = Direction (1);
  SCM reldir = me->get_property ("side-relative-direction");
  if (is_direction (reldir))
    relative_dir = to_dir (reldir);

  SCM other_elt = me->get_object ("direction-source");
  Grob *e = unsmob_grob (other_elt);
  if (e)
    return (Direction) (relative_dir * get_grob_direction (e));

  return CENTER;
}

/* Put the element next to the support, optionally taking in
   account the extent of the support.  */
SCM
Side_position_interface::general_side_position (Grob *me, Axis a, bool use_extents)
{
  Real ss = Staff_symbol_referencer::staff_space (me);

  extract_grob_set (me, "side-support-elements", support);

  Grob *common = common_refpoint_of_array (support, me->get_parent (a), a);
  Grob *staff_symbol = Staff_symbol_referencer::get_staff_symbol (me);
  bool include_staff = 
    staff_symbol
    && a == Y_AXIS
    && scm_is_number (me->get_property ("staff-padding"))
    && !to_boolean (me->get_property ("quantize-position"));

  Interval dim;
  Interval staff_extents;
  if (include_staff)
    {
      common = staff_symbol->common_refpoint (common, Y_AXIS);
      staff_extents = staff_symbol->extent (common, Y_AXIS);

      if (include_staff)
	dim.unite (staff_extents);
    }

  for (int i = 0; i < support.size (); i++)
    {
      Grob *e = support[i];
      if (e)
	if (use_extents)
	  dim.unite (e->extent (common, a));
	else
	  {
	    Real x = e->relative_coordinate (common, a);
	    dim.unite (Interval (x, x));
	  }
    }

  if (dim.is_empty ())
    dim = Interval (0, 0);

  Direction dir = get_grob_direction (me);

  Real off = me->get_parent (a)->relative_coordinate (common, a);
  Real minimum_space = ss * robust_scm2double (me->get_property ("minimum-space"), -1);

  Real total_off = dim.linear_combination (dir) - off;
  total_off += dir * ss * robust_scm2double (me->get_property ("padding"), 0);

  if (minimum_space >= 0
      && dir
      && total_off * dir < minimum_space)
    total_off = minimum_space * dir;
  
  /* FIXME: 100CM should relate to paper size.  */
  if (fabs (total_off) > 100 CM)
    {
      String msg
	= String_convert::form_string ("Improbable offset for grob %s: %f%s",
				       me->name ().to_str0 (), total_off,
				       INTERNAL_UNIT);

      programming_error (msg);
    }
  return scm_from_double (total_off);
}


MAKE_SCHEME_CALLBACK (Side_position_interface, y_aligned_on_support_refpoints, 1);
SCM
Side_position_interface::y_aligned_on_support_refpoints (SCM smob)
{
  return general_side_position (unsmob_grob (smob), Y_AXIS, false); 
}



/*
  Position next to support, taking into account my own dimensions and padding.
*/

MAKE_SCHEME_CALLBACK (Side_position_interface, x_aligned_side, 1);
SCM
Side_position_interface::x_aligned_side (SCM smob)
{
  return aligned_side (unsmob_grob (smob), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Side_position_interface, y_aligned_side, 1);
SCM
Side_position_interface::y_aligned_side (SCM smob)
{
  return aligned_side (unsmob_grob (smob), Y_AXIS);
}

SCM
Side_position_interface::aligned_side (Grob*me, Axis a)
{
  Direction dir = get_grob_direction (me);

  Real o = scm_to_double (general_side_position (me, a, true));
  Interval iv = me->extent (me, a);

  if (!iv.is_empty ())
    {
      if (!dir)
	{
	  programming_error ("direction unknown, but aligned-side wanted");
	  dir = DOWN;
	}
      o += -iv[-dir];
    }

  /*
    Maintain a minimum distance to the staff. This is similar to side
    position with padding, but it will put adjoining objects on a row if
    stuff sticks out of the staff a little.
  */
  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  if (staff && a == Y_AXIS)
    {
      if (to_boolean (me->get_property ("quantize-position")))
	{
	  Grob *common = me->common_refpoint (staff, Y_AXIS);
	  Real my_off = me->relative_coordinate (common, Y_AXIS);
	  Real staff_off = staff->relative_coordinate (common, Y_AXIS);
	  Real ss = Staff_symbol::staff_space (staff);
	  Real position = 2 * (my_off + o - staff_off) / ss;
	  Real rounded = directed_round (position, dir);
	  Grob *head = me->get_parent (X_AXIS);
      
	  if (rounded <= Staff_symbol_referencer::staff_radius (me) 
	      || (Note_head::has_interface (head)
		  && sign (Staff_symbol_referencer::get_position (head)) == - dir))
	    {
	      o += dir *(rounded - position) * 0.5 * ss;
	      if (Staff_symbol_referencer::on_staffline (me, int (rounded)))
		o += dir * 0.5 * ss;
	    }
	}
      else if (scm_is_number (me->get_property ("staff-padding")))
	{
	  Real padding
	    = Staff_symbol_referencer::staff_space (me)
	    * scm_to_double (me->get_property ("staff-padding"));

	  Grob *common = me->common_refpoint (staff, Y_AXIS);

	  Interval staff_size = staff->extent (common, Y_AXIS);
	  Real diff = dir*staff_size[dir] + padding - dir * (o + iv[-dir]);
	  o += dir * max (diff, 0.0);
	}
    }
  return scm_from_double (o);
}

void
Side_position_interface::set_axis (Grob *me, Axis a)
{
  if (!scm_is_number (me->get_property ("side-axis")))
    {
      me->set_property ("side-axis", scm_from_int (a));
      add_offset_callback (me,
			   (a==X_AXIS)
			   ? x_aligned_side_proc
			   : y_aligned_side_proc,
			   a);
    }
}
Axis
Side_position_interface::get_axis (Grob *me)
{
  if (scm_is_number (me->get_property ("side-axis")))
    return Axis (scm_to_int (me->get_property ("side-axis")));
  
  programming_error ("side-axis not set.");
  return NO_AXES;
}

ADD_INTERFACE (Side_position_interface, "side-position-interface",
	       "Position a victim object (this one) next to other objects (the "
	       "support).   The property @code{direction} signifies where to put the  "
	       "victim object relative to the support (left or right, up or down?)\n\n "
	       "The routine also takes the size the staff into account if "
	       "@code{staff-padding} is set. If undefined, the staff symbol is ignored.",

	       /* properties */
	       "direction "
	       "direction-source "
	       "minimum-space "
	       "padding "
	       "side-axis "
	       "side-relative-direction "
	       "side-support-elements "
	       "slur-padding "
	       "staff-padding "
	       "quantize-position "
	       );
