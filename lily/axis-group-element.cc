/*
  axis-group-element.cc -- implement Axis_group_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axis-group-element.hh"
#include "dimension-cache.hh"
#include "group-interface.hh"

Link_array<Score_element>
Axis_group_element::get_extra_dependencies() const
{
  Link_array<Score_element> e(elem_l_arr ());
  return e;
}

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
  axes_[0] = (Axis)-1 ; 
  axes_[1] = (Axis)-1 ;

  set_elt_property ("elements", SCM_EOL);
  set_elt_property ("transparent", SCM_BOOL_T);
}

void
Axis_group_element::set_axes (Axis a1, Axis a2)
{
  axes_[0] = a1 ; 
  axes_[1] = a2 ;
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



void
Axis_group_element::add_element (Score_element *e)
{
  used_b_ =true;
  e->used_b_ = true;
  
  for (int i = 0; i < 2; i++)
    {
      if (!e->parent_l (axes_[i]))
	e->set_parent (this, axes_[i]);
    }
  Group_interface gi (this);
  gi.add_element (e);
}

