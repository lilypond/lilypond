/*   
  staff-side.cc --  implement Staff_side_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "staff-side.hh"
#include "staff-symbol.hh"
#include "debug.hh"
#include "warn.hh"
#include "dimensions.hh"
#include "dimension-cache.hh"

Staff_sidify::Staff_sidify (Score_element *e)
{
  elt_l_ = e;
}


void
Staff_sidify::add_support (Score_element*e)
{
  SCM sup = elt_l_->get_elt_property ("side-support");
  elt_l_->set_elt_property ("side-support",
			    gh_cons (e->self_scm_,sup));
}

Real
Staff_sidify::aligned_position (Dimension_cache const *c)
{
  return position_self (c);
}


Direction
Staff_sidify::get_direction () const
{
  SCM d = elt_l_->get_elt_property ("direction");
  if (isdir_b (d))
    return to_dir (d) ? to_dir (d) : DOWN;

  Direction relative_dir = UP;
  SCM reldir = elt_l_->get_elt_property ("side-relative-direction");	// should use a lambda.
  if (isdir_b (d))
    {
      relative_dir = to_dir (reldir);
    }
  
  SCM other_elt = elt_l_->get_elt_property ("direction-source");
  if (SMOB_IS_TYPE_B (Score_element, other_elt))
    {
      Score_element * e = SMOB_TO_TYPE(Score_element,other_elt);

      return relative_dir * Staff_sidify (e).get_direction ();
    }
  
  return DOWN;
}
  
/**
   Callback that does the aligning.
 */
Real
Staff_sidify::position_self (Dimension_cache const * c)
{
  Score_element * me = dynamic_cast<Score_element*> (c->element_l ());

  Interval dim;
  Axis  axis = c->axis ();
  Graphical_element *common = me->parent_l (axis);
  SCM support = me->get_elt_property ("side-support");
  for (SCM s = support; s != SCM_EOL; s = gh_cdr (s))
    {
      assert (SMOB_IS_TYPE_B (Score_element, gh_car (s)));
      Score_element * e  = SMOB_TO_TYPE(Score_element, gh_car (s));
      common = common->common_refpoint (e, axis);
    }
  
  for (SCM s = support; s != SCM_EOL; s = gh_cdr (s))
    {
      Score_element * e  = SMOB_TO_TYPE(Score_element, gh_car (s));
      Real coord = e->relative_coordinate (common, axis);

      dim.unite (coord + e->extent (axis));
    }


  if (dim.empty_b ())
    {
      dim = Interval(0,0);
    }

  Real off =  me->parent_l (axis)->relative_coordinate (common, axis);


  Direction dir = Staff_sidify (me).get_direction ();
    
  SCM pad = me->remove_elt_property ("padding");
  if (pad != SCM_UNDEFINED)
    {
      off += gh_scm2double (pad) * dir;
    }
  Real total_off = dim[dir] + off;

  if (fabs (total_off) > 100 CM)
    programming_error ("Huh ? Improbable staff side dim.");

  return total_off;
}

void
Staff_sidify::set_axis (Axis a)
{
  if (elt_l_->get_elt_property ("transparent") == SCM_UNDEFINED)
    elt_l_->set_elt_property ("side-support" ,SCM_EOL);

  Axis other = Axis ((a +1)%2);
  elt_l_->dim_cache_[a]->set_offset_callback (position_self);
  elt_l_->dim_cache_[other]->set_offset_callback (0);  
}

Axis
Staff_sidify::get_axis () const
{
  if (elt_l_->dim_cache_[X_AXIS]->off_callback_l_ == position_self) // UGH.
    return X_AXIS;
  else
    return Y_AXIS;  
}

void
Staff_sidify::set_direction (Direction d) 
{
  elt_l_->set_elt_property ("direction", gh_int2scm (d));
}

bool
Staff_sidify::is_staff_side_b ()
{
  return elt_l_->get_elt_property ("side-support") != SCM_UNDEFINED;
}
