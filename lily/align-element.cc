/*
  align-elem.cc -- implement Align_elem

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "align-element.hh"
#include "interval.hh"
#include "direction.hh"
#include "debug.hh"
#include "hash-table-iter.hh"
#include "dimension-cache.hh"

struct Align_element_content {
  Graphical_element * elem_l_;
  int priority_i_;
  
  static int compare (Align_element_content const &h1, 
		      Align_element_content const &h2) 
    {
      return h1.priority_i_ - h2.priority_i_;
    }
  Align_element_content (Graphical_element *elem_l, int p) 
    {
      priority_i_ = p;
      elem_l_ = elem_l;
    }
  Align_element_content () {
    elem_l_ = 0;
    priority_i_ = 0;
  }
};



void
Align_element::add_element (Score_element*el_l)
{
  int p = elem_l_arr_.size ();
  add_element_priority (el_l, p);
}

void
Align_element::add_element_priority (Score_element *el, int p)
{
  assert (! contains_b (el));
  Axis_group_element::add_element (el);
  priority_i_hash_[el] = p;
  add_dependency (el);
}

void
Align_element::do_substitute_element_pointer (Score_element*o,
					      Score_element*n)
{
  Axis_group_element :: do_substitute_element_pointer (o,n);
  if (o == center_l_)
    {
      center_l_ = n;
    }
  if (priority_i_hash_.elem_b (o))
    {
      priority_i_hash_[n] = priority_i_hash_[o];
      /*
	Huh? It seems the old pointers are still used.  Why?
       */
      // priority_i_hash_.remove (o);
    }
}

void
Align_element::do_post_processing()
{
  if (axis () == Y_AXIS)
    do_side_processing ();
}

void
Align_element::do_pre_processing ()
{
  if (axis () == X_AXIS)
    do_side_processing ();
}

void
Align_element::do_side_processing ()
{
  Array<Interval> dims;

  Link_array<Score_element> elems;
  for (int i=0; i < elem_l_arr_.size(); i++) 
    {
      Interval y = elem_l_arr_[i]->extent(axis ()) + elem_l_arr_[i]->relative_coordinate (this, axis ());
      if (!y.empty_b())
	{
	  Score_element *e =dynamic_cast<Score_element*>(elem_l_arr_[i]);

	  // todo: fucks up if item both in Halign & Valign. 
	  SCM min_dims = e->remove_elt_property (minimum_space_scm_sym);
	  if (min_dims != SCM_BOOL_F)
	    {
	      min_dims = SCM_CDR (min_dims);
	      y.unite (Interval (gh_scm2double (SCM_CAR (min_dims)),
				 gh_scm2double (SCM_CDR (min_dims))));
	    }
	  
	  SCM extra_dims = e->remove_elt_property (extra_space_scm_sym);
	  if (extra_dims != SCM_BOOL_F)
	    {
	      extra_dims = SCM_CDR (extra_dims);
	      y[LEFT] += gh_scm2double (SCM_CAR (extra_dims));
	      y[RIGHT] += gh_scm2double (SCM_CDR (extra_dims));
	    }

	  elems.push (e);
	  dims.push (y);	  
	}
    }

  Real where_f=0;
  Real center_f = 0.0;
  for (int i=0 ;  i < elems.size(); i++) 
    {
      Real dy = - stacking_dir_ * dims[i][-stacking_dir_];
      if (i)
	dy += stacking_dir_ * dims[i-1][stacking_dir_];

      if (i)
	{
	  dy = (dy >? threshold_interval_[SMALLER] )
	    <? threshold_interval_[BIGGER];
	}

      if (!i && align_dir_ == LEFT)
	center_f = where_f;
      else if (align_dir_ == CENTER && elems[i] == center_l_)
	center_f = where_f;

      where_f += stacking_dir_ * dy;
      elems[i]->translate_axis (where_f, axis ());
    }

  if (dims.size ())
    where_f += dims.top ()[stacking_dir_];
  if (align_dir_ == RIGHT)
    center_f = where_f;
  else if (align_dir_ == CENTER && !center_l_)
    center_f = where_f / 2;
    
  if (center_f)
    translate_axis ( - center_f, axis ());

  dim_cache_[axis ()]->invalidate ();
}

Align_element::Align_element()
{
  ordered_b_ = true;
  threshold_interval_ = Interval (0, Interval::infinity ());
  stacking_dir_ = DOWN;
  align_dir_ = CENTER;
  center_l_ =0;
  priority_i_hash_.hash_func_ = pointer_hash;
}

Axis
Align_element::axis () const
{
  return axes_[0];
}

void
Align_element::set_axis (Axis a)
{
  set_axes (a,a);
}


bool
Align_element::contains_b (Score_element const *e) const
{
  return elem_l_arr_.find_l (e);
}

void
Align_element::sort_elements ()
{
  Array<Align_element_content> content;
  for  (int i =0; i < elem_l_arr_.size(); i++)
    {
      Score_element * e = dynamic_cast<Score_element*> (elem_l_arr_[i]);
      assert (priority_i_hash_.elem_b (e));
      int p = priority_i_hash_[e];
      content.push (Align_element_content (e, p));
    }
  content.sort (Align_element_content::compare);
  
  elem_l_arr_.clear();
  priority_i_hash_.clear();

  for  (int i =0; i < content.size(); i++) 
    {
      elem_l_arr_.push (content[i].elem_l_);
    }
}

void
Align_element::do_print () const
{
#ifndef NPRINT
  DOUT << "contains: ";
  for (int i=0 ;  i < elem_l_arr_.size(); i++) 
    DOUT << classname (elem_l_arr_[i]) << ", ";
#endif
}

Score_element*
Align_element::get_elt_by_priority (int p) const
{
  for (Hash_table_iter<Score_element*, int>  i(priority_i_hash_); i.ok (); i++)
    {
      if (i.val () == p)
	return i.key();
    }
  return 0;
}

int
Align_element::get_priority (Score_element const * e) const
{
  Score_element * nonconst = (Score_element*) e;
  if ( priority_i_hash_.elem_b (nonconst))
    return priority_i_hash_[nonconst];
  else
    return elem_l_arr_.find_i (nonconst);
}
