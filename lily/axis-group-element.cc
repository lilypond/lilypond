/*
  axis-group-element.cc -- implement Axis_group_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axis-group-element.hh"
#include "dimension-cache.hh"
#include "group-interface.hh"

Link_array<Score_element>
Axis_group_element::elem_l_arr () const
{  
  return
    Group_interface__extract_elements (this, (Score_element*)0, "elements");
}

Link_array<Score_element> 
Axis_group_element::get_children ()
{
  Link_array<Score_element> childs;
  Link_array<Score_element> elems = elem_l_arr ();

  for (int i=0; i < elems.size (); i++) 
    {
      Score_element* e = elems[i];
      childs.push (e) ;
      Axis_group_element * axis_group= dynamic_cast <Axis_group_element *> (e);
      if (axis_group)
      	childs.concat (axis_group->get_children ());      
    }
  
  return childs;
}

Axis_group_element::Axis_group_element()
{
  set_elt_property ("elements", SCM_EOL);
  set_elt_property ("transparent", SCM_BOOL_T);
}

void
Axis_group_element::set_axes (Axis a1, Axis a2)
{
  SCM ax = gh_cons (gh_int2scm (a1), SCM_EOL);
  if (a1 != a2)
    ax= gh_cons (gh_int2scm (a2), ax);

  
  set_elt_property ("axes", ax);

  if (a1 != X_AXIS && a2 != X_AXIS)
    set_empty (X_AXIS);
  if (a1 != Y_AXIS && a2 != Y_AXIS)
    set_empty (Y_AXIS);
  
  dim_cache_[a1]->set_callback(extent_callback);
  dim_cache_[a2]->set_callback (extent_callback);
}

Interval
Axis_group_element::extent_callback (Dimension_cache const *c) 
{
  Axis a = c->axis ();
  Axis_group_element * me
    = dynamic_cast<Axis_group_element*> (c->element_l ()); 

  Interval r;
  for (SCM s = me->get_elt_property ("elements"); gh_pair_p (s); s = gh_cdr (s))
    {
      SCM e=gh_car (s); 
      Score_element * se = SMOB_TO_TYPE (Score_element, e);

      Interval dims = se->extent (a);
      if (!dims.empty_b ())
	r.unite (dims + se->relative_coordinate (me, a));
    }

  return r;
}


bool
Axis_group_element::axis_b (Axis a )const
{
  return dim_cache_[a]->extent_callback_l_ == extent_callback;
}


void
Axis_group_element::add_element (Score_element *e)
{
  used_b_ =true;
  e->used_b_ = true;

  for (SCM ax = get_elt_property ("axes"); ax != SCM_EOL ; ax = gh_cdr (ax))
    {
      Axis a = (Axis) gh_scm2int (gh_car (ax));
      
      if (!e->parent_l (a))
	e->set_parent (this, a);
    }
  Group_interface gi (this);
  gi.add_element (e);

  add_dependency (e);
}

