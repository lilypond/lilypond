/*   
  staff-side.cc --  implement Staff_side_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include <math.h>		// ceil.

#include "side-position-interface.hh"
#include "debug.hh"
#include "warn.hh"
#include "dimensions.hh"

#include "staff-symbol-referencer.hh"
#include "group-interface.hh"

void
Side_position::add_support (Score_element*me, Score_element*e)
{
  Pointer_group_interface (me, "side-support-elements").add_element (e);
}



Direction
Side_position::get_direction (Score_element*me)
{
  SCM d = me->get_elt_property ("direction");
  if (isdir_b (d))
    return to_dir (d) ? to_dir (d) : DOWN;

  Direction relative_dir = UP;
  SCM reldir = me->get_elt_property ("side-relative-direction");	// should use a lambda.
  if (isdir_b (reldir))
    {
      relative_dir = to_dir (reldir);
    }
  
  SCM other_elt = me->get_elt_property ("direction-source");
  Score_element * e = unsmob_element(other_elt);
  if (e)
    {
      return (Direction)(relative_dir * Side_position::get_direction (e));
    }
  
  return DOWN;
}
  
/*
   Callback that does the aligning. Puts the element next to the support
 */

Real
Side_position::side_position (Score_element *cme, Axis axis)
{
  Score_element* me = (Score_element*)cme;
  Score_element *common = me->parent_l (axis);
  SCM support = me->get_elt_property ("side-support-elements");
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

  Direction dir = Side_position::get_direction (me);
    
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
Side_position::aligned_on_self (Score_element *elm, Axis ax)
{
  String s ("self-alignment-");

  s +=  (ax == X_AXIS) ? "X" : "Y";

  SCM align (elm->get_elt_property (s.ch_C()));
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
Side_position::quantised_position (Score_element *me, Axis )
{
  Direction d = Side_position::get_direction (me);

  if (Staff_symbol_referencer::has_interface (me))
    {
      Real p = Staff_symbol_referencer::position_f (me);
      Real rp = directed_round (p, d);

      int ip = int  (rp);
      if ((ip % 2) == 0)
	{
	  ip += d;
	  rp += d;
	}

      return (rp - p) * Staff_symbol_referencer::staff_space (me) / 2.0;
    }
  return 0.0;
}

/*
  Position next to support, taking into account my own dimensions and padding.
 */
Real
Side_position::aligned_side (Score_element *me, Axis ax)
{
  
  Direction d = Side_position ::get_direction (me);
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
Side_position::centered_on_parent (Score_element * me, Axis a)
{
  Score_element *him = me->parent_l (a);

  return him->extent (a).center ();  
}


void
Side_position::add_staff_support (Score_element*me)
{
  Score_element* st = Staff_symbol_referencer::staff_symbol_l (me);
  if (st)
    {
      add_support (me,st);
    }
}

void
Side_position::set_axis (Score_element*me, Axis a)
{
  // prop transparent ? 
  if (me->get_elt_property ("side-support-elements") == SCM_UNDEFINED)
    me->set_elt_property ("side-support-elements" ,SCM_EOL);

  if (!me->has_offset_callback_b (aligned_side, a))
    me->add_offset_callback (aligned_side, a);
}




Axis
Side_position::get_axis (Score_element*me)
{
  if (me->has_offset_callback_b (&side_position, X_AXIS)
      || me->has_offset_callback_b (&aligned_side , X_AXIS))
    return X_AXIS;

  
  return Y_AXIS;
}

void
Side_position::set_direction (Score_element*me, Direction d)
{
  me->set_elt_property ("direction", gh_int2scm (d));
}

void
Side_position::set_minimum_space (Score_element*me, Real m)
{
  me->set_elt_property ("minimum-space", gh_double2scm (m));
}

void
Side_position::set_padding (Score_element*me, Real p)
{
  me->set_elt_property ("padding", gh_double2scm (p));
}

bool
Side_position::has_interface (Score_element*me) 
{
  return me->get_elt_property ("side-support-elements") != SCM_UNDEFINED;
}

bool
Side_position::supported_b (Score_element*me) 
{
  SCM s =me->get_elt_property  ("side-support-elements"); 
  return s != SCM_UNDEFINED && s != SCM_EOL;
}


