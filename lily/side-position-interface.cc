/*   
  staff-side.cc --  implement Staff_side_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include <math.h>		// ceil.

#include "side-position-interface.hh"
#include "staff-symbol.hh"
#include "debug.hh"
#include "warn.hh"
#include "dimensions.hh"
#include "dimension-cache.hh"
#include "staff-symbol-referencer.hh"
#include "group-interface.hh"

Side_position_interface::Side_position_interface (Score_element const *e)
{
  elt_l_ = (Score_element*)e;
}

void
Side_position_interface::add_support (Score_element*e)
{
  Pointer_group_interface (elt_l_, "side-support-elements").add_element (e);
}



Direction
Side_position_interface::get_direction () const
{
  SCM d = elt_l_->get_elt_property ("direction");
  if (isdir_b (d))
    return to_dir (d) ? to_dir (d) : DOWN;

  Direction relative_dir = UP;
  SCM reldir = elt_l_->get_elt_property ("side-relative-direction");	// should use a lambda.
  if (isdir_b (reldir))
    {
      relative_dir = to_dir (reldir);
    }
  
  SCM other_elt = elt_l_->get_elt_pointer ("direction-source");
  Score_element * e = unsmob_element(other_elt);
  if (e)
    {
      return (Direction)(relative_dir * Side_position_interface (e).get_direction ());
    }
  
  return DOWN;
}
  
/*
   Callback that does the aligning. Puts the element next to the support
 */

Real
Side_position_interface::side_position (Score_element *cme, Axis axis)
{
  Score_element* me = (Score_element*)cme;
  Score_element *common = me->parent_l (axis);
  SCM support = me->get_elt_pointer ("side-support-elements");
  for (SCM s = support; s != SCM_EOL; s = gh_cdr (s))
    {
      Score_element * e  = unsmob_element (gh_car (s));
      if (e)
	common = common->common_refpoint (e, axis);
    }
  
  Interval dim;
  for (SCM s = support; s != SCM_EOL; s = gh_cdr (s))
    {

      Score_element * e  = unsmob_element ( gh_car (s));
      if (e)
	{
	  Real coord = e->relative_coordinate (common, axis);

	  dim.unite (coord + e->extent (axis));
	}
    }

  if (dim.empty_b ())
    {
      dim = Interval(0,0);
    }

  Direction dir = Side_position_interface (me).get_direction ();
    
  Real off =  me->parent_l (axis)->relative_coordinate (common, axis);
  SCM minimum = me->remove_elt_property ("minimum-space");

  Real total_off = dim[dir] + off;
  SCM padding = me->remove_elt_property ("padding");
  if (gh_number_p (padding))
    {
      total_off += gh_scm2double (padding) * dir;
    }
  if (gh_number_p (minimum) && total_off * dir < gh_scm2double (minimum))
    {
      total_off = gh_scm2double (minimum) * dir;
    }
  if (fabs (total_off) > 100 CM)
    programming_error ("Huh ? Improbable staff side dim.");

  return total_off;
}

/**
  callback that centers the element on itself
 */
Real
Side_position_interface::aligned_on_self (Score_element *elm, Axis ax)
{
  String s ("self-alignment-");

  s +=  (ax == X_AXIS) ? "X" : "Y";

  SCM align (elm->get_elt_property (s));
  if (isdir_b (align))
    {
      Direction d = to_dir (align);
      Interval ext(elm->extent (ax));

      if (ext.empty_b ())
	{
	  programming_error ("I'm empty. Can't align on self");
	  return 0.0;
	}
      else if (d)
	{
	  return - ext[d];
	}
      return - ext.center ();
    }
  else
    return 0.0;
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
  of the elements "direction" elt property. */
Real
Side_position_interface::quantised_position (Score_element *me, Axis a)
{
  Side_position_interface s(me);
  Direction d = s.get_direction ();

  if (Staff_symbol_referencer_interface::has_interface_b (me))
    {
      Staff_symbol_referencer_interface si (me);
      Real p = si.position_f ();
      Real rp = directed_round (p, d);

      int ip = int  (rp);
      if ((ip % 2) == 0)
	{
	  ip += d;
	  rp += d;
	}

      return (rp - p) * si.staff_space () / 2.0;
    }
  return 0.0;
}

/*
  Position next to support, taking into account my own dimensions and padding.
 */
Real
Side_position_interface::aligned_side (Score_element *me, Axis ax)
{
  Side_position_interface s(me);
  Direction d = s.get_direction ();
  Real o = side_position (me,ax);

  Interval iv =  me->extent (ax);

  if (!iv.empty_b ())
    {
      o += - iv[-d];

      SCM pad = me->get_elt_property ("padding");
      if (gh_number_p (pad))
	o += d *gh_scm2double (pad) ; 
    }
  return o;
}

/*
  Position centered on parent.
 */
Real
Side_position_interface::centered_on_parent (Score_element * me, Axis a)
{
  Score_element *him = me->parent_l (a);

  return him->extent (a).center ();  
}


void
Side_position_interface::add_staff_support ()
{
  Staff_symbol_referencer_interface si (elt_l_);
  if (si.staff_symbol_l ())
    {
      add_support (si.staff_symbol_l ());
    }
}

void
Side_position_interface::set_axis (Axis a)
{
  // prop transparent ? 
  if (elt_l_->get_elt_pointer ("side-support-elements") == SCM_UNDEFINED)
    elt_l_->set_elt_pointer ("side-support-elements" ,SCM_EOL);

  if (!elt_l_->has_offset_callback_b (aligned_side, a))
    elt_l_->add_offset_callback (aligned_side, a);
}


void
Side_position_interface::set_quantised (Axis a)
{
  elt_l_->add_offset_callback (quantised_position, a);
}

Axis
Side_position_interface::get_axis () const
{
  if (elt_l_->has_offset_callback_b (&side_position, X_AXIS)
      || elt_l_->has_offset_callback_b (&aligned_side , X_AXIS))
    return X_AXIS;

  
  return Y_AXIS;
}

void
Side_position_interface::set_direction (Direction d) 
{
  elt_l_->set_elt_property ("direction", gh_int2scm (d));
}

void
Side_position_interface::set_minimum_space (Real m)
{
  elt_l_->set_elt_property ("minimum-space", gh_double2scm (m));
}

void
Side_position_interface::set_padding (Real p)
{
  elt_l_->set_elt_property ("padding", gh_double2scm (p));
}

bool
Side_position_interface::has_interface_b () const
{
  return elt_l_->get_elt_pointer ("side-support-elements") != SCM_UNDEFINED;
}

bool
Side_position_interface::supported_b () const
{
  SCM s =elt_l_->get_elt_pointer  ("side-support-elements"); 
  return s != SCM_UNDEFINED && s != SCM_EOL;
}


