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

/*
  This callback is set in the children of the align element. It does
  not compute anything, but a side effect of a->do_side_processing ()
  is that the elements are placed correctly.  */
Real
Align_element::alignment_callback (Dimension_cache const *c)
{
  Axis ax = c->axis ();
  Score_element * sc = c->element_l ()->parent_l (ax);
  Align_element * a = dynamic_cast<Align_element*> (sc);
  if (a && a->get_elt_property ("alignment-done") == SCM_UNDEFINED) 
    {
      a->do_side_processing (ax);
    }
  return 0.0;
}

void
Align_element::add_element (Score_element* s)
{
  s->add_offset_callback (alignment_callback, axis ());
  Axis_group_element::add_element (s);
}

/*
  Hairy function to put elements where they should be. Can be tweaked
  from the outside by setting minimum-space and extra-space in its
  children */
void
Align_element::do_side_processing (Axis a)
{
  set_elt_property ("alignment-done", SCM_BOOL_T);
  
  SCM d = get_elt_property ("stacking-dir");
  Direction stacking_dir = gh_number_p(d) ? to_dir (d) : CENTER;
  if (!stacking_dir)
    stacking_dir = DOWN;

  
  Array<Interval> dims;

  Link_array<Score_element> elems;
  Link_array<Score_element> all_elts (elem_l_arr ());
  for (int i=0; i < all_elts.size(); i++) 
    {
      Interval y = all_elts[i]->extent(a) + all_elts[i]->relative_coordinate (this, a);
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

  
  Interval threshold = Interval (0, Interval::infinity ());
  SCM thr = get_elt_property ("threshold");
  if (gh_pair_p (thr))
    {
      threshold[SMALLER] = gh_scm2double (gh_car (thr));
      threshold[BIGGER] = gh_scm2double (gh_cdr (thr));      
    }

  Real where_f=0;
  for (int i=0 ;  i < elems.size(); i++) 
    {
      Real dy = - stacking_dir * dims[i][-stacking_dir];
      if (i)
	dy += stacking_dir * dims[i-1][stacking_dir];

      if (i)
	{
	  dy = (dy >? threshold[SMALLER] )
	    <? threshold[BIGGER];
	}

      where_f += stacking_dir * dy;
      elems[i]->translate_axis (where_f, a);
    }
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
  return Axis (gh_scm2int (gh_car (get_elt_property ("axes"))));
}

void
Align_element::set_axis (Axis a)
{
  set_axes (a, a);
}






