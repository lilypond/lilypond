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

Side_position_interface::Side_position_interface (Score_element const *e)
{
  elt_l_ = (Score_element*)e;
}


void
Side_position_interface::add_support (Score_element*e)
{
  SCM sup = elt_l_->get_elt_property ("side-support");
  elt_l_->set_elt_property ("side-support",
			    gh_cons (e->self_scm_,sup));
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
  
  SCM other_elt = elt_l_->get_elt_property ("direction-source");
  Score_element * e = unsmob_element(other_elt);
  if (e)
    {
      return (Direction)(relative_dir * Side_position_interface (e).get_direction ());
    }
  
  return DOWN;
}
  
/**
   Callback that does the aligning.
 */
Real
Side_position_interface::side_position (Dimension_cache const * c)
{
  Score_element * me = dynamic_cast<Score_element*> (c->element_l ());

  Interval dim;
  Axis  axis = c->axis ();
  Score_element *common = me->parent_l (axis);
  SCM support = me->get_elt_property ("side-support");
  for (SCM s = support; s != SCM_EOL; s = gh_cdr (s))
    {
      Score_element * e  = unsmob_element ( gh_car (s));
      if (e)
	common = common->common_refpoint (e, axis);
    }
  
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

  Real off =  me->parent_l (axis)->relative_coordinate (common, axis);


  Direction dir = Side_position_interface (me).get_direction ();
    
  SCM pad = me->remove_elt_property ("padding");
  if (gh_number_p (pad))
    {
      off += gh_scm2double (pad) * dir;
    }
  Real total_off = dim[dir] + off;

  if (fabs (total_off) > 100 CM)
    programming_error ("Huh ? Improbable staff side dim.");

  return total_off;
}

Real
Side_position_interface::self_alignment (Dimension_cache const *c)
{
  String s ("self-alignment-");
  Axis ax = c->axis ();
  s +=  (ax == X_AXIS) ? "X" : "Y";
  Score_element *elm = dynamic_cast<Score_element*> (c->element_l ());
  SCM align (elm->get_elt_property (s));
  if (isdir_b (align))
    {
      Direction d = to_dir (align);
      Interval ext(elm->extent (ax));
      if (d)
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

Real
Side_position_interface::quantised_position (Dimension_cache const *c)
{
  Score_element * me = dynamic_cast<Score_element*> (c->element_l ());
  Side_position_interface s(me);
  Direction d = s.get_direction ();
  Staff_symbol_referencer_interface si (me);

  if (si.has_interface_b ())
    {
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

Real
Side_position_interface::aligned_side (Dimension_cache const *c)
{
  Score_element * me = dynamic_cast<Score_element*> (c->element_l ());
  Side_position_interface s(me);
  Direction d = s.get_direction ();
  Axis ax = c->axis ();
  Real o = side_position (c);

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
  if (elt_l_->get_elt_property ("side-support") == SCM_UNDEFINED)
    elt_l_->set_elt_property ("side-support" ,SCM_EOL);

  elt_l_->dim_cache_[a]->off_callbacks_.push (aligned_side);
}


void
Side_position_interface::set_quantised (Axis a)
{
  Dimension_cache * c = elt_l_->dim_cache_[a];
  
  c->off_callbacks_.push (quantised_position);
}

Axis
Side_position_interface::get_axis () const
{
  Dimension_cache * c =  elt_l_->dim_cache_[X_AXIS];
  for (int i=0 ; i < c->off_callbacks_.size();i ++)
    if (c->off_callbacks_[i] == side_position
	||c->off_callbacks_[i] == aligned_side)
      return X_AXIS;

  
  return Y_AXIS;
}

void
Side_position_interface::set_direction (Direction d) 
{
  elt_l_->set_elt_property ("direction", gh_int2scm (d));
}

bool
Side_position_interface::has_interface_b () const
{
  return elt_l_->get_elt_property ("side-support") != SCM_UNDEFINED;
}

bool
Side_position_interface::supported_b () const
{
  SCM s =elt_l_->get_elt_property  ("side-support"); 
  return s != SCM_UNDEFINED && s != SCM_EOL;
}


Side_position_interface
side_position (Score_element* e)
{
  Side_position_interface si (e);
  return si;
}
