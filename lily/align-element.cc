/*
  align-elem.cc -- implement Align_elem

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "align-element.hh"
#include "interval.hh"
#include "direction.hh"
#include "debug.hh"
#include "hash-table-iter.hh"
#include "dimension-cache.hh"

void
Align_element::after_line_breaking()
{
  if (axis () == Y_AXIS)
    do_side_processing ();
}

void
Align_element::before_line_breaking ()
{
  if (axis () == X_AXIS)
    do_side_processing ();
}

void
Align_element::do_side_processing ()
{
  SCM d = get_elt_property ("stacking-dir");
  Direction stacking_dir = gh_number_p(d) ? to_dir (d) : CENTER;
  if (!stacking_dir)
    stacking_dir = DOWN;

  
  Array<Interval> dims;

  Link_array<Score_element> elems;
  Link_array<Score_element> all_elts (elem_l_arr ());
  for (int i=0; i < all_elts.size(); i++) 
    {
      Interval y = all_elts[i]->extent(axis ()) + all_elts[i]->relative_coordinate (this, axis ());
      if (!y.empty_b())
	{
	  Score_element *e =dynamic_cast<Score_element*>(all_elts[i]);

	  // todo: fucks up if item both in Halign & Valign. 
	  SCM min_dims = e->remove_elt_property ("minimum-space");
	  if (gh_pair_p (min_dims) &&
	      gh_number_p (gh_car (min_dims))
	      && gh_number_p (gh_cdr (min_dims)))
	    {
	      y.unite (Interval (gh_scm2double (gh_car  (min_dims)),
				 gh_scm2double (gh_cdr (min_dims))));
	    }
	  
	  SCM extra_dims = e->remove_elt_property ("extra-space");
	  if (gh_pair_p (extra_dims) &&
	      gh_number_p (gh_car (extra_dims))
	      && gh_number_p (gh_cdr (extra_dims)))
	    {
	      y[LEFT] += gh_scm2double (gh_car  (extra_dims));
	      y[RIGHT] += gh_scm2double (gh_cdr (extra_dims));
	    }

	  elems.push (e);
	  dims.push (y);	  
	}
    }

  Real where_f=0;
  for (int i=0 ;  i < elems.size(); i++) 
    {
      Real dy = - stacking_dir * dims[i][-stacking_dir];
      if (i)
	dy += stacking_dir * dims[i-1][stacking_dir];

      if (i)
	{
	  dy = (dy >? threshold_interval_[SMALLER] )
	    <? threshold_interval_[BIGGER];
	}


      where_f += stacking_dir * dy;
      elems[i]->translate_axis (where_f, axis ());
    }
}

Align_element::Align_element()
{
  threshold_interval_ = Interval (0, Interval::infinity ());
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






