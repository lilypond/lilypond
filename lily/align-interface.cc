/*   
  align-interface.cc --  implement Align_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "align-interface.hh"
#include "grob.hh"
#include "group-interface.hh"
#include "axis-group-interface.hh"
#include "hara-kiri-group-spanner.hh"
#include "paper-def.hh"

MAKE_SCHEME_CALLBACK (Align_interface,alignment_callback,2);
SCM
Align_interface::alignment_callback (SCM element_smob, SCM axis)
{
  Grob * me = unsmob_grob (element_smob);
  Axis ax = (Axis)gh_scm2int (axis);
  Grob * par = me->get_parent (ax);
  if (par && !to_boolean (par->get_grob_property ("alignment-done")))
    {
      Align_interface::align_elements_to_extents (par, ax);
    }
  return gh_double2scm (0.0);
}

MAKE_SCHEME_CALLBACK (Align_interface,fixed_distance_alignment_callback,2);
SCM
Align_interface::fixed_distance_alignment_callback (SCM element_smob, SCM axis)
{
  Grob * me = unsmob_grob (element_smob);
  Axis ax = (Axis)gh_scm2int (axis);
  Grob * par = me->get_parent (ax);
  if (par && !to_boolean (par->get_grob_property ("alignment-done")))
    {
      Align_interface::align_to_fixed_distance (par, ax);
    }
  return gh_double2scm (0.0);
}

/*
  merge with align-to-extents? 
 */
void
Align_interface::align_to_fixed_distance (Grob *me , Axis a)
{
  me->set_grob_property ("alignment-done", SCM_BOOL_T);
  
  SCM d =   me->get_grob_property ("stacking-dir");
  
  Direction stacking_dir = gh_number_p (d) ? to_dir (d) : CENTER;
  if (!stacking_dir)
    stacking_dir = DOWN;

  SCM force = me->get_grob_property ("forced-distance");

  Real dy = 0.0;
  if (gh_number_p (force))
    {
      dy = gh_scm2double (force);
    }
  
  Link_array<Grob> elems
    = Pointer_group_interface__extract_elements (me, (Grob*) 0, "elements");

  Real where_f=0;

  Interval v;
  v.set_empty ();
  Array<Real> translates;
  
  for (int j= elems.size (); j--; ) 
    {
      /*
	This is not very elegant, in that we need special support for
	hara kiri. Unfortunately, the generic wiring of
	force_hara_kiri_callback () (extent and offset callback) is
	such that we might get into a loop if we call extent() or
	offset() the elements.
	
	 
       */
      if (a == Y_AXIS
	  && Hara_kiri_group_spanner::has_interface (elems[j]))
	Hara_kiri_group_spanner::consider_suicide (elems[j]);

      if (!ly_pair_p (elems[j]-> immutable_property_alist_))
	elems.del(j);
    }

  for (int j =0; j < elems.size (); j++)
    {
      where_f += stacking_dir * dy;
      translates.push (where_f);
      v.unite (Interval (where_f, where_f));
    }

  /*
    TODO: support self-alignment-{Y,X}
   */
  for (int i = 0; i < translates.size (); i++)
    {
      elems[i]->translate_axis (translates[i] - v.center (), a);
    }
}

/*
  Hairy function to put elements where they should be. Can be tweaked
  from the outside by setting minimum-space and extra-space in its
  children

  We assume that the children the refpoints of the children are still
  found at 0.0 -- we will fuck up with thresholds if children's
  extents are already moved to locations such as (-16, -8), since the
  dy needed to put things in a row doesn't relate to the distances
  between original refpoints.

  TODO: maybe we should rethink and throw out thresholding altogether.
  The original function has been taken over by
  align_to_fixed_distance().
*/
void
Align_interface::align_elements_to_extents (Grob * me, Axis a)
{
  me->set_grob_property ("alignment-done", SCM_BOOL_T);
  
  SCM d =   me->get_grob_property ("stacking-dir");

  
  Direction stacking_dir = gh_number_p (d) ? to_dir (d) : CENTER;
  if (!stacking_dir)
    stacking_dir = DOWN;


  
  Interval threshold = Interval (0, Interval::infinity ());
  SCM thr = me->get_grob_property ("threshold");
  if (gh_pair_p (thr))
    {
      threshold[SMALLER] = gh_scm2double (ly_car (thr));
      threshold[BIGGER] = gh_scm2double (ly_cdr (thr));      
    }

  
  Array<Interval> dims;

  Link_array<Grob> elems;
  Link_array<Grob> all_grobs
    = Pointer_group_interface__extract_elements (me, (Grob*) 0, "elements");
  for (int i=0; i < all_grobs.size (); i++) 
    {
      Interval y = all_grobs[i]->extent (me, a);
      if (!y.empty_b ())
	{
	  Grob *e =dynamic_cast<Grob*> (all_grobs[i]);

	  // todo: fucks up if item both in Halign & Valign. 
	  SCM min_dims = e->remove_grob_property ("minimum-space");
	  if (gh_pair_p (min_dims) &&
	      gh_number_p (ly_car (min_dims))
	      && gh_number_p (ly_cdr (min_dims)))
	    {
	      y.unite (ly_scm2interval (min_dims));
	    }
	  
	  SCM extra_dims = e->remove_grob_property ("extra-space");
	  if (gh_pair_p (extra_dims) &&
	      gh_number_p (ly_car (extra_dims))
	      && gh_number_p (ly_cdr (extra_dims)))
	    {
	      y[LEFT] += gh_scm2double (ly_car (extra_dims));
	      y[RIGHT] += gh_scm2double (ly_cdr (extra_dims));
	    }

	  elems.push (e);
	  dims.push (y);	  
	}
    }
  
 
  /*
    Read self-alignment-X and self-alignment-Y. This may seem like
    code duplication. (and really: it is), but this is necessary to
    prevent ugly cyclic dependencies that arise when you combine
    self-alignment on a child with alignment of children.
  */

  String s ("self-alignment-");

  s += (a == X_AXIS) ? "X" : "Y";

  SCM align (me->get_grob_property (s.ch_C ()));
     
  Array<Real> translates ;
  Interval total;
  Real where_f=0;
  
  for (int j=0 ;  j < elems.size (); j++) 
    {
      Real dy = -  dims[j][-stacking_dir];
      if (j)
	dy += dims[j-1][stacking_dir];


      /*
	we want dy to be > 0
       */
      dy *= stacking_dir; 
      if (j)
	{
	  dy = (dy >? threshold[SMALLER])
	    <? threshold[BIGGER];
	}

      where_f += stacking_dir * dy;
      total.unite (dims[j] +   where_f);
      translates.push (where_f);
    }

  
  Grob * align_center = unsmob_grob (align);
  Real center_offset = 0.0;
  
  /*
    also move the grobs that were empty, to maintain spatial order. 
   */
  Array<Real> all_translates;
  if (translates.size ())
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
	  if (all_grobs[j] == align_center)
	    center_offset = w;
	  all_translates .push (w);
	  j++;
	}


      /*
	FIXME: uncommenting freaks out the Y-alignment of
	line-of-score.
       */
      // Real align_param = isdir_b (align)  ? gh_scm2double (align) : 0.0;
      
      if (gh_number_p (align))
	center_offset = total.linear_combination (gh_scm2double (align));

      for (int j = 0 ;  j < all_grobs.size (); j++)
	all_grobs[j]->translate_axis (all_translates[j] - center_offset, a);
    }
}
Axis
Align_interface::axis (Grob*me)
{
  return  Axis (gh_scm2int (ly_car (me->get_grob_property ("axes"))));
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
      if (ly_car (e) == s->self_scm ())
	break;
      c++;
      e = ly_cdr (e);
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
  Axis_group_interface::set_axes (me, a,a);
}

bool
Align_interface::has_interface (Grob*me)
{
  return me && me->has_interface (ly_symbol2scm ("align-interface"));
}

