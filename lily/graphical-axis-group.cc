/*
  axis-group.cc -- implement Graphical_axis_group

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dimension-cache.hh"
#include "interval.hh"
#include "graphical-axis-group.hh"
#include "axis-group-element.hh"
#include "graphical-element.hh"
#include "debug.hh"

/** don't copy anything: an element can only be in one
  Graphical_axis_group at one time. */
Graphical_axis_group::Graphical_axis_group(Graphical_axis_group const&s)
{
  axes_[0] = s.axes_[0];
  axes_[1] = s.axes_[1];
  ordered_b_ = s.ordered_b_;
}

bool 
Graphical_axis_group::contains_b (Graphical_element const *e) const
{
  return elem_l_arr_.find_l (e);
}

Interval
Graphical_axis_group::extent (Axis axis) const
{
  Interval r;
  for (int i=0; i < elem_l_arr_.size(); i++)
    {
      r.unite (elem_l_arr_[i]->extent (axis)
	       + elem_l_arr_[i]->relative_coordinate (this, axis)
	       );
    }
  return r;
}

void
Graphical_axis_group::add_element (Graphical_element*e, Axis a1 , Axis a2)
{
  used_b_ =true;
  e->used_b_ = true;

  Axis as[2] = {
    (a1 == NO_AXES) ? axes_[0] : a1,
    (a2 == NO_AXES) ? axes_[1] : a2,    
  };

    
  
  for (int i = 0; i < 2; i++)
    {
      if (e->parent_l (as[i]))
	continue;
      
      e->set_parent (this, as[i]);

      e->dim_cache_[as[i]]->dependencies_l_arr_.push (dim_cache_[as[i]]);
    }
  assert (e->parent_l(Y_AXIS) == this || e->parent_l (X_AXIS) == this);
  elem_l_arr_.push (e);
}


/**
   ugr. duplication of functionality with remove_all ()
 */
void
Graphical_axis_group::remove_element (Graphical_element*e)
{
  assert (contains_b (e));
  if (ordered_b_)
    elem_l_arr_.substitute (e,0);
  else
    elem_l_arr_.unordered_substitute (e,0);

  do_remove (e);
}

void
Graphical_axis_group::do_remove (Graphical_element *e)
{
  for (int i=0; i<  2; i++)
    {
      Axis a=axes_[i];
      if (e->parent_l (a) != this)
	continue;
      e->set_parent (0, a);
      e->dim_cache_[a]->dependencies_l_arr_.clear ();
    }
}

void
Graphical_axis_group::remove_all ()
{
  for (int i=0; i < elem_l_arr_.size(); i++) 
    do_remove (elem_l_arr_[i]);

  elem_l_arr_.clear ();
}


void    
Graphical_axis_group::do_print() const
{
#ifndef NPRINT
  for (int i=0; i < elem_l_arr_.size(); i++) 
    DEBUG_OUT << classname(elem_l_arr_[i]) << " ";
#endif
}

Graphical_axis_group::Graphical_axis_group ()
{
  ordered_b_ = false;
  axes_[0] = (Axis)-1 ; 
  axes_[1] = (Axis)-1 ;
}

void
Graphical_axis_group::set_axes (Axis a1, Axis a2)
{
  axes_[0] = a1 ; 
  axes_[1] = a2 ;
}
