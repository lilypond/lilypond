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
  Pointer_group_interface::add_element (me, "side-support-elements",e);
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

MAKE_SCHEME_CALLBACK(Side_position,side_position,2);
SCM
Side_position::side_position (SCM element_smob, SCM axis)
{
  Score_element *me = unsmob_element (element_smob);
  Axis a = (Axis) gh_scm2int (axis);

  Score_element *common = me->parent_l (a);
  SCM support = me->get_elt_property ("side-support-elements");
  for (SCM s = support; s != SCM_EOL; s = gh_cdr (s))
    {
      Score_element * e  = unsmob_element (gh_car (s));
      if (e)
	common = common->common_refpoint (e, a);
    }
  
  Interval dim;
  for (SCM s = support; s != SCM_EOL; s = gh_cdr (s))
    {

      Score_element * e  = unsmob_element ( gh_car (s));
      if (e)
	{
	  dim.unite (e->extent (common, a));
	}
    }

  if (dim.empty_b ())
    {
      dim = Interval(0,0);
    }

  Direction dir = Side_position::get_direction (me);
    
  Real off =  me->parent_l (a)->relative_coordinate (common, a);
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

  return gh_double2scm (total_off);
}

/**
  callback that centers the element on itself
 */
MAKE_SCHEME_CALLBACK(Side_position,aligned_on_self,2);
SCM
Side_position::aligned_on_self (SCM element_smob, SCM axis)
{
  Score_element *me = unsmob_element (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  String s ("self-alignment-");

  s +=  (a == X_AXIS) ? "X" : "Y";

  SCM align (me->get_elt_property (s.ch_C()));
  if (gh_number_p (align))
    {
      Interval ext(me->extent (me,a));

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
  else if (unsmob_element (align))
    {
      return gh_double2scm (- unsmob_element (align)->relative_coordinate (me,  a));
    }
    return gh_double2scm (0.0);
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
  Staff_symbol_referencer::staff_radius() */
MAKE_SCHEME_CALLBACK(Side_position,quantised_position,2);
SCM
Side_position::quantised_position (SCM element_smob, SCM )
{
  Score_element *me = unsmob_element (element_smob);
  
  
  Direction d = Side_position::get_direction (me);

  if (Staff_symbol_referencer::has_interface (me))
    {
      Real p = Staff_symbol_referencer::position_f (me);
      Real rp = directed_round (p, d);
      Real rad = Staff_symbol_referencer::staff_radius (me) *2 ;
      int ip = int  (rp);

      if (abs (ip) < rad && Staff_symbol_referencer::on_staffline (me,ip))
	{
	  ip += d;
	  rp += d;
	}

      return gh_double2scm ((rp - p) * Staff_symbol_referencer::staff_space (me) / 2.0);
    }
  return gh_double2scm (0.0);
}

/*
  Position next to support, taking into account my own dimensions and padding.
 */
MAKE_SCHEME_CALLBACK(Side_position,aligned_side,2);
SCM
Side_position::aligned_side (SCM element_smob, SCM axis)
{
  Score_element *me = unsmob_element (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  
  Direction d = Side_position::get_direction (me);
  Real o = gh_scm2double (side_position (element_smob,axis));

  Interval iv =  me->extent (me, a);

  if (!iv.empty_b ())
    {
      o += - iv[-d];

      SCM pad = me->get_elt_property ("padding");
      if (gh_number_p (pad))
	o += d *gh_scm2double (pad) ; 
    }
  return gh_double2scm (o);
}

/*
  Position centered on parent.
 */
MAKE_SCHEME_CALLBACK(Side_position,centered_on_parent,2);
SCM
Side_position::centered_on_parent (SCM element_smob, SCM axis)
{
  Score_element *me = unsmob_element (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  Score_element *him = me->parent_l (a);

  return gh_double2scm (him->extent (him,a).center ());  
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
  me->add_offset_callback (Side_position::aligned_side_proc, a);
}



// ugh. doesn't cactch all variants. 
Axis
Side_position::get_axis (Score_element*me)
{
  if (me->has_offset_callback_b (Side_position::aligned_side_proc, X_AXIS)
      || me->has_offset_callback_b (Side_position::aligned_side_proc , X_AXIS))
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
  return me->has_interface (ly_symbol2scm ("side-position-interface"));
}

bool
Side_position::supported_b (Score_element*me) 
{
  SCM s = me->get_elt_property  ("side-support-elements"); 
  return gh_pair_p(s);
}


