/*
  side-position-interface.cc -- implement Side_position_interface

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "side-position-interface.hh"

#include <math.h>		// ceil.

#include "note-head.hh"
#include "warn.hh"
#include "warn.hh"
#include "dimensions.hh"
#include "staff-symbol-referencer.hh"
#include "group-interface.hh"
#include "directional-element-interface.hh"
#include "staff-symbol-referencer.hh"
#include "string-convert.hh"

void
Side_position_interface::add_support (Grob *me, Grob *e)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("side-support-elements"), e);
}

Direction
Side_position_interface::get_direction (Grob *me)
{
  SCM d = me->get_property ("direction");
  if (is_direction (d) && to_dir (d))
    return to_dir (d);

  Direction relative_dir = Direction (1);
  SCM reldir = me->get_property ("side-relative-direction");	// should use a lambda.
  if (is_direction (reldir))
    {
      relative_dir = to_dir (reldir);
    }

  SCM other_elt = me->get_property ("direction-source");
  Grob *e = unsmob_grob (other_elt);
  if (e)
    {
      return (Direction) (relative_dir * get_grob_direction (e));
    }

  return CENTER;
}

MAKE_SCHEME_CALLBACK (Side_position_interface, aligned_on_support_extents, 2);
SCM
Side_position_interface::aligned_on_support_extents (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) scm_to_int (axis);

  return general_side_position (me, a, true);
}

/* Put the element next to the support, optionally taking in
   account the extent of the support.  */
SCM
Side_position_interface::general_side_position (Grob *me, Axis a, bool use_extents)
{
  Real ss = Staff_symbol_referencer::staff_space (me);
  SCM support = me->get_property ("side-support-elements");
  Grob *common = common_refpoint_of_list (support, me->get_parent (a), a);
  Grob *st = Staff_symbol_referencer::get_staff_symbol (me);
  bool include_staff = (st
			&& a == Y_AXIS
			&& scm_is_number (me->get_property ("staff-padding")));

  Interval dim;
  if (include_staff)
    {
      common = st->common_refpoint (common, Y_AXIS);
      dim = st->extent (common, Y_AXIS);
    }

  for (SCM s = support; s != SCM_EOL; s = scm_cdr (s))
    {
      Grob *e = unsmob_grob (scm_car (s));
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

  Direction dir = Side_position_interface::get_direction (me);

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
  return scm_make_real (total_off);
}

/*
  Cut & paste (ugh.)
*/
MAKE_SCHEME_CALLBACK (Side_position_interface, aligned_on_support_refpoints, 2);
SCM
Side_position_interface::aligned_on_support_refpoints (SCM smob, SCM axis)
{
  Grob *me = unsmob_grob (smob);
  Axis a = (Axis) scm_to_int (axis);

  return general_side_position (me, a, false);
}

Real
directed_round (Real f, Direction d)
{
  if (d < 0)
    return floor (f);
  else
    return ceil (f);
}

/*
  Callback that quantises in staff-spaces, rounding in the direction
  of the elements "direction" elt property.

  Only rounds when we're inside the staff, as determined by
  Staff_symbol_referencer::staff_radius () */
MAKE_SCHEME_CALLBACK (Side_position_interface, quantised_position, 2);
SCM
Side_position_interface::quantised_position (SCM element_smob, SCM)
{
  Grob *me = unsmob_grob (element_smob);

  Direction d = Side_position_interface::get_direction (me);

  Grob *stsym = Staff_symbol_referencer::get_staff_symbol (me);
  if (stsym)
    {
      Real p = Staff_symbol_referencer::get_position (me);
      Real rp = directed_round (p, d);
      Real rad = Staff_symbol_referencer::staff_radius (me) * 2;
      int ip = int (rp);

      Grob *head = me->get_parent (X_AXIS);

      if (Staff_symbol_referencer::on_staffline (me, ip)
	  && ((abs (ip) <= rad)
	      || (Note_head::has_interface (head)
		  && sign (Staff_symbol_referencer::get_position (head))
		  == -d)))
	{
	  ip += d;
	  rp += d;
	}

      return scm_make_real ((rp - p) * Staff_symbol_referencer::staff_space (me) / 2.0);
    }
  return scm_make_real (0.0);
}

/*
  Position next to support, taking into account my own dimensions and padding.
*/
MAKE_SCHEME_CALLBACK (Side_position_interface, aligned_side, 2);
SCM
Side_position_interface::aligned_side (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) scm_to_int (axis);

  Direction d = Side_position_interface::get_direction (me);

  Real o = scm_to_double (aligned_on_support_extents (element_smob, axis));

  Interval iv = me->extent (me, a);

  if (!iv.is_empty ())
    {
      if (!d)
	{
	  programming_error ("Direction unknown, but aligned-side wanted.");
	  d = DOWN;
	}
      o += -iv[-d];
    }

  /*
    Maintain a minimum distance to the staff. This is similar to side
    position with padding, but it will put adjoining objects on a row if
    stuff sticks out of the staff a little.
  */
  Grob *st = Staff_symbol_referencer::get_staff_symbol (me);
  if (st && a == Y_AXIS
      && scm_is_number (me->get_property ("staff-padding")))
    {
      Real padding
	= Staff_symbol_referencer::staff_space (me)
	* scm_to_double (me->get_property ("staff-padding"));

      Grob *common = me->common_refpoint (st, Y_AXIS);

      Interval staff_size = st->extent (common, Y_AXIS);
      Real diff = d*staff_size[d] + padding - d * (o + iv[-d]);
      o += (d * (diff >? 0));
    }

  return scm_make_real (o);
}

void
Side_position_interface::set_axis (Grob *me, Axis a)
{
  me->add_offset_callback (Side_position_interface::aligned_side_proc, a);
}

// ugh. doesn't catch all variants. 
Axis
Side_position_interface::get_axis (Grob *me)
{
  if (me->has_offset_callback (Side_position_interface::aligned_side_proc, X_AXIS)
      || me->has_offset_callback (Side_position_interface::aligned_side_proc, X_AXIS))
    return X_AXIS;

  return Y_AXIS;
}

ADD_INTERFACE (Side_position_interface, "side-position-interface",
	       "Position a victim object (this one) next to other objects (the "
	       "support).   The property @code{direction} signifies where to put the  "
	       "victim object relative to the support (left or right, up or down?)\n\n "
	       "The routine also takes the size the staff into account if "
	       "@code{staff-padding} is set. If undefined, the staff symbol is ignored.",
	       "staff-padding side-support-elements direction-source "
	       "direction side-relative-direction minimum-space padding");
