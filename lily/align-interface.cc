/*   
  align-interface.cc --  implement Align_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "align-interface.hh"
#include "grob.hh"
#include "group-interface.hh"
#include "axis-group-interface.hh"
#include "paper-def.hh"

/*
  This callback is set in the children of the align element. It does
  not compute anything, but a side effect of a->do_side_processing ()
  is that the elements are placed correctly.  */
MAKE_SCHEME_CALLBACK(Align_interface,alignment_callback,2);
SCM
Align_interface::alignment_callback (SCM element_smob, SCM axis)
{
  Grob * me = unsmob_grob (element_smob);
  Axis ax = (Axis )gh_scm2int (axis);
  Grob * par = me->parent_l (ax);
  if (par && !to_boolean (par->get_grob_property ("alignment-done")))
    {
      Align_interface::align_to_extents (par, ax);
    }
  return gh_double2scm (0.0);
}

MAKE_SCHEME_CALLBACK(Align_interface,fixed_distance_alignment_callback,2);
SCM
Align_interface::fixed_distance_alignment_callback (SCM element_smob, SCM axis)
{
  Grob * me = unsmob_grob (element_smob);
  Axis ax = (Axis )gh_scm2int (axis);
  Grob * par = me->parent_l (ax);
  if (par && !to_boolean (par->get_grob_property ("alignment-done")))
    {
      Align_interface::align_to_fixed_distance (par, ax);
    }
  return gh_double2scm (0.0);
}

void
Align_interface::align_to_fixed_distance (Grob *me , Axis a)
{
  me->set_grob_property ("alignment-done", SCM_BOOL_T);
  
  SCM d =   me->get_grob_property ("stacking-dir");

  
  Direction stacking_dir = gh_number_p(d) ? to_dir (d) : CENTER;
  if (!stacking_dir)
    stacking_dir = DOWN;


  SCM force = me->get_grob_property ("forced-distance");

  Real dy = 0.0;
  if (gh_number_p (force))
    {
      dy = gh_scm2double (force);
    }
  
  Link_array<Grob> elems
    = Pointer_group_interface__extract_elements (  me, (Grob*) 0, "elements");
  Real where_f=0;
  for (int j=0 ;  j < elems.size(); j++) 
    {
      where_f += stacking_dir * dy;
      elems[j]->translate_axis (where_f, a);
    }
}

/*
  Hairy function to put elements where they should be. Can be tweaked
  from the outside by setting minimum-space and extra-space in its
  children */
void
Align_interface::align_to_extents (Grob * me, Axis a)
{
  me->set_grob_property ("alignment-done", SCM_BOOL_T);
  
  SCM d =   me->get_grob_property ("stacking-dir");

  
  Direction stacking_dir = gh_number_p(d) ? to_dir (d) : CENTER;
  if (!stacking_dir)
    stacking_dir = DOWN;


  
  Interval threshold = Interval (0, Interval::infinity ());
  SCM thr = me->get_grob_property ("threshold");
  if (gh_pair_p (thr))
    {
      threshold[SMALLER] = gh_scm2double (gh_car (thr));
      threshold[BIGGER] = gh_scm2double (gh_cdr (thr));      
    }

  
  Array<Interval> dims;

  Link_array<Grob> elems;
  Link_array<Grob> all_grobs
    = Pointer_group_interface__extract_elements (  me, (Grob*) 0, "elements");
  for (int i=0; i < all_grobs.size(); i++) 
    {
      Interval y = all_grobs[i]->extent(me, a);
      if (!y.empty_b())
	{
	  Grob *e =dynamic_cast<Grob*>(all_grobs[i]);

	  // todo: fucks up if item both in Halign & Valign. 
	  SCM min_dims = e->remove_grob_property ("minimum-space");
	  if (gh_pair_p (min_dims) &&
	      gh_number_p (gh_car (min_dims))
	      && gh_number_p (gh_cdr (min_dims)))
	    {
	      y.unite (ly_scm2interval (min_dims));
	      
	    }
	  
	  SCM extra_dims = e->remove_grob_property ("extra-space");
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
  Array<Real> translates ;
  for (int j=0 ;  j < elems.size(); j++) 
    {
      Real dy = 0.0;
      dy = - stacking_dir * dims[j][-stacking_dir];
      if (j)
	dy += stacking_dir * dims[j-1][stacking_dir];
      
      if (j)
	{
	  dy = (dy >? threshold[SMALLER] )
	    <? threshold[BIGGER];
	}

      where_f += stacking_dir * dy;
      translates.push (where_f);
    }

  /*
    also move the grobs that were empty, to maintain spatial order. 
   */
  if (translates.size  ())
    {
      int i =0;
      int j =0;
      Real w = translates[0];
      while (j  < all_grobs.size ())
	{
	  if (i < elems.size () && all_grobs[j] == elems[i])
	    {
	      w = translates[i++];
	    }
	  all_grobs[j]->translate_axis (w, a);

	  j++;
	}
    }
}

Axis
Align_interface::axis (Grob*me)
{
  return  Axis (gh_scm2int (gh_car (me->get_grob_property ("axes"))));
}


/*
  should  use generic Scm funcs.
 */
int
Align_interface::get_count (Grob*me,Grob*s)
{
  SCM e = me->get_grob_property ("elements");
  int c =0;
  while (gh_pair_p (e))
    {
      if (gh_car (e) == s->self_scm ())
	break;
      c++;
      e = gh_cdr (e);
    }
  return c;
}

void
Align_interface::add_element (Grob*me,Grob* s, SCM cb)
{
  s->add_offset_callback (cb, Align_interface::axis (me));
  Axis_group_interface::add_element (me, s);
}


void
Align_interface::set_interface (Grob*me)
{
  me->set_interface (ly_symbol2scm ("align-interface"));

  Axis_group_interface::set_interface (me);
}

void
Align_interface::set_axis (Grob*me,Axis a)
{
  Axis_group_interface::set_axes (me, a,a );
}

bool
Align_interface::has_interface (Grob*me)
{
  return me && me->has_interface (ly_symbol2scm ("align-interface"));
}

