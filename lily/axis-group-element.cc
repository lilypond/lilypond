/*
  axis-group-element.cc -- implement Axis_group_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axis-group-element.hh"
#include "graphical-axis-group.hh"
#include "dimension-cache.hh"

Link_array<Score_element>
Axis_group_element::get_extra_dependencies() const
{
  Link_array<Score_element> e(elem_l_arr ());
  return e;
}

Link_array<Score_element>
Axis_group_element::elem_l_arr () const
{  
  /*
    ugh. I know
  */
  Link_array<Score_element> r;
  for (int i=0; i < elem_l_arr_.size (); i++)
    r.push (dynamic_cast<Score_element*>(elem_l_arr_[i]));
      
  return r;
}

Link_array<Score_element> 
Axis_group_element::get_children ()
{
  Link_array<Score_element> childs;
  Link_array<Score_element> elems = elem_l_arr ();

  elems.concat (extra_elems_ );
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

void
Axis_group_element::do_print() const
{
  Graphical_axis_group::do_print();
}

Axis_group_element::Axis_group_element()
{
  set_elt_property (transparent_scm_sym, SCM_BOOL_T);
}

void
Axis_group_element::set_axes (Axis a1, Axis a2)
{
  Graphical_axis_group::set_axes (a1,a2);
  set_empty (a1 != X_AXIS && a2 != X_AXIS, X_AXIS);
  set_empty (a1 != Y_AXIS && a2 != Y_AXIS, Y_AXIS);
}


void
Axis_group_element::do_substitute_element_pointer (Score_element*o,
						   Score_element*n)
{
  int i;
  Graphical_element * go = o;
  Graphical_element * gn = n;  
  
  while ((i = elem_l_arr_.find_i (go))>=0)
    elem_l_arr_.substitute (go,gn);

  extra_elems_.substitute (o, n);
}

Interval
Axis_group_element::extra_extent (Axis a )const
{
  Interval g;
  Axis_group_element* urg = (Axis_group_element*)this;
  urg->purge_extra ();		// Yeah yeah,  const correctness.
  for (int i=0;  i < extra_elems_.size (); i++)
    {
      Interval ge = extra_elems_[i]->relative_coordinate (this, a)
	+ extra_elems_[i]->extent (a);
      g.unite (ge);
    }
  return g;
}

Interval
Axis_group_element::do_height () const
{
  Interval gag = Graphical_axis_group::extent (Y_AXIS);
  gag.unite (extra_extent (Y_AXIS));
  return gag;
}

Interval
Axis_group_element::do_width () const
{
  Interval gag = Graphical_axis_group::extent (X_AXIS);
  gag.unite (extra_extent (X_AXIS));
  return gag;
}


/*
  UGH.
 */
void
Axis_group_element::add_extra_element (Score_element *e)
{
  Link_array<Score_element> se;
  while (e && e != this)
    {
      se.push (e);
      e = dynamic_cast<Score_element*> (e->parent_l (Y_AXIS));
    }

  if (1)			// e == this)
    {
      for (int i=0; i < se.size( ); i++) 
	{
	  extra_elems_.push (se[i]);
	  add_dependency (se[i]);
	  se[i]->set_elt_property (ly_symbol ("Axis_group_element::add_extra_element"), SCM_BOOL_T); // UGH GUH.
	}
      
    }
}

/*
  UGH GUH
 */
void
Axis_group_element::purge_extra ()
{
  for (int i=0; i < extra_elems_.size ();)
    {
      Score_element *e = extra_elems_[i];
      while (e && e != this)
	{
	  e = dynamic_cast<Score_element*> (e->parent_l (Y_AXIS));
	}
      if (e != this)
	extra_elems_.del (i);
      else
	i++;
    }
}

Interval
Axis_group_element::extent (Axis a) const
{
  return Graphical_element::extent (a);
}
  
