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
  Link_array<Score_element> all_elts (elem_l_arr ());
  for (int i=0; i < elem_l_arr ().size(); i++) 
    {
      Interval y = all_elts[i]->extent(axis ()) + all_elts[i]->relative_coordinate (this, axis ());
      if (!y.empty_b())
	{
	  Score_element *e =dynamic_cast<Score_element*>(all_elts[i]);

	  // todo: fucks up if item both in Halign & Valign. 
	  SCM min_dims = e->remove_elt_property ("minimum-space");
	  if (min_dims != SCM_UNDEFINED)
	    {
	      y.unite (Interval (gh_scm2double (SCM_CAR (min_dims)),
				 gh_scm2double (SCM_CDR (min_dims))));
	    }
	  
	  SCM extra_dims = e->remove_elt_property ("extra-space");
	  if (extra_dims != SCM_UNDEFINED)
	    {
	      y[LEFT] += gh_scm2double (SCM_CAR (extra_dims));
	      y[RIGHT] += gh_scm2double (SCM_CDR (extra_dims));
	    }

	  elems.push (e);
	  dims.push (y);	  
	}
    }

  Real where_f=0;
  Real center_f = 0.0;
  SCM scenter = get_elt_property ("center-element");
  Score_element *center_elt = unsmob_element (scenter);
  
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
      else if (align_dir_ == CENTER && elems[i] == center_elt)
	center_f = where_f;

      where_f += stacking_dir_ * dy;
      elems[i]->translate_axis (where_f, axis ());
    }

  if (dims.size ())
    where_f += dims.top ()[stacking_dir_];
  if (align_dir_ == RIGHT)
    center_f = where_f;
  else if (align_dir_ == CENTER && !center_elt)
    center_f = where_f / 2;
    
  if (center_f)
    translate_axis ( - center_f, axis ());


  //  dim_cache_[axis ()]->invalidate ();

}

Align_element::Align_element()
{
  threshold_interval_ = Interval (0, Interval::infinity ());
  stacking_dir_ = DOWN;
  align_dir_ = CENTER;
}

int
Align_element::get_count (Score_element*s)const
{
  SCM e = get_elt_property ("elements");
  int c =0;
  while (gh_pair_p (e))
    {
      if (gh_car (e) == s->self_scm_)
	break;
      c++;
      e = gh_cdr (e);
    }
  return c;
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






