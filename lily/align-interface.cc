/*   
  align-interface.cc --  implement Align_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "align-interface.hh"

#include "score-element.hh"
#include "group-interface.hh"
#include "axis-group-interface.hh"

/*
  This callback is set in the children of the align element. It does
  not compute anything, but a side effect of a->do_side_processing ()
  is that the elements are placed correctly.  */
Real
Align_interface::alignment_callback (Score_element *sc, Axis ax)
{
  Score_element * par = sc->parent_l (ax);
  if (par && par->get_elt_property ("alignment-done") == SCM_UNDEFINED) 
    {
      Align_interface::do_side_processing (par, ax);
    }
  return 0.0;
}


Real
Align_interface::center_on_element (Score_element *me, Axis a)
{
  Score_element *cent = unsmob_element (me->get_elt_property ("group-center-element"));

  if (cent)
    {
      Real r = cent->relative_coordinate (me,  a);
      return -r;
    }
  return 0;
}

/*
  Hairy function to put elements where they should be. Can be tweaked
  from the outside by setting minimum-space and extra-space in its
  children */
void
Align_interface::do_side_processing (Score_element * me, Axis a)
{
  me->set_elt_property ("alignment-done", SCM_BOOL_T);
  
  SCM d =   me->get_elt_property ("stacking-dir");
  Direction stacking_dir = gh_number_p(d) ? to_dir (d) : CENTER;
  if (!stacking_dir)
    stacking_dir = DOWN;

  
  Array<Interval> dims;

  Link_array<Score_element> elems;
  Link_array<Score_element> all_elts
    = Pointer_group_interface__extract_elements (  me, (Score_element*) 0, "elements");
  for (int i=0; i < all_elts.size(); i++) 
    {
      Interval y = all_elts[i]->extent(a) + all_elts[i]->relative_coordinate (me, a);
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
  SCM thr = me->get_elt_property ("threshold");
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


Axis
Align_interface::axis (Score_element*me)
{
  return  Axis (gh_scm2int (gh_car (me->get_elt_property ("axes"))));
}


/*
  should  use generic Scm funcs.
 */
int
Align_interface::get_count (Score_element*me,Score_element*s)
{
  SCM e = me->get_elt_property ("elements");
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

void
Align_interface::add_element (Score_element*me,Score_element* s)
{
  s->add_offset_callback (alignment_callback, Align_interface::axis (me));
  Axis_group_interface::add_element (me, s);
}


void
Align_interface::set_interface (Score_element*me)
{
  me->set_interface (ly_symbol2scm ("align-interface"));

  Axis_group_interface::set_interface (me);
}

void
Align_interface::set_axis (Score_element*me,Axis a)
{
  Axis_group_interface::set_axes (me, a,a );
}

bool
Align_interface::has_interface (Score_element*me)
{
  return me && me->has_interface (ly_symbol2scm ("align-interface"));
}

