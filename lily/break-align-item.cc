/*
  break-align-item.cc -- implement Break_align_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dimension-cache.hh"
#include "lily-guile.hh"
#include "break-align-item.hh"
#include "dimensions.hh"
#include "paper-score.hh"
#include "paper-def.hh"
#include "paper-column.hh"

/*
  Handle spacing for prefatory matter. 

  TODO: rewrite this.  It is kludgy
*/

void
Break_align_item::do_pre_processing()
{
  if (break_status_dir() == LEFT)
    align_dir_ = LEFT;
  else
    align_dir_ = RIGHT;
  
  flip (&align_dir_);

  Real interline= paper_l ()->get_var ("interline");	
  
  Link_array<Score_element> elems;
  Link_array<Score_element> all_elems (elem_l_arr ());
  
  for (int i=0; i < all_elems.size(); i++) 
    {
      Interval y = all_elems[i]->extent(axis ());
      if (!y.empty_b())
	elems.push (dynamic_cast<Score_element*> (all_elems[i]));
    }
  
  if (!elems.size ())
    return;

  SCM symbol_list = SCM_EOL;
  Array<Real> dists;
  SCM current_origin = ly_str02scm ("");
  for (int i=0; i <= elems.size (); i++)
    {
      Score_element *next_elt  = i < elems.size ()
	? elems[i]
	: 0 ;
      
      SCM next_origin;

      if (next_elt)
	{
	  next_origin = next_elt->get_elt_property ("origin");
	  next_origin =
	    (next_origin == SCM_UNDEFINED)
	    ? ly_str02scm ("")
	    : next_origin;
	}
      else
	next_origin = ly_str02scm ("begin-of-note");
      
      SCM extra_space
	= scm_eval (scm_listify (ly_symbol2scm ("break-align-spacer"),
				 current_origin, next_origin, SCM_UNDEFINED)); 
      SCM symbol = gh_car  (extra_space);
      Real spc = gh_scm2double (SCM_CADR(extra_space));
      spc *= interline;

      dists.push(spc);
      symbol_list = gh_cons (symbol, symbol_list);
      current_origin = next_origin;
    }


  // skip the first sym.
  symbol_list  = gh_cdr (scm_reverse (symbol_list));
  for (int i=0; i <elems.size()-1; i++)
    {
      String sym_str = ly_scm2string (gh_car  (symbol_list));
      elems[i]->set_elt_property (sym_str,
				  scm_cons (gh_double2scm (0),
					    gh_double2scm (dists[i+1])));

      symbol_list = gh_cdr (symbol_list);
    }


  // urg
  SCM first_pair = elems[0]->get_elt_property ("minimum-space");
  if (gh_pair_p (first_pair))
    first_pair = first_pair;
  else
    first_pair = gh_cons (gh_double2scm (0.0), gh_double2scm (0.0));
  
  scm_set_car_x (first_pair, gh_double2scm (-dists[0]));
  elems[0]->set_elt_property ("minimum-space", first_pair);
  
  Axis_align_item::do_pre_processing();


  Real pre_space = elems[0]->extent (X_AXIS)[LEFT]
    + elems[0]->relative_coordinate (column_l (), X_AXIS);
  Real spring_len = elems.top ()->extent (X_AXIS)[RIGHT]
    + elems.top ()->relative_coordinate (column_l (), X_AXIS);
  
  Real stretch_distance =0.;
  
  if (gh_car  (symbol_list) == ly_symbol2scm ("extra-space"))
    {
      spring_len += dists.top ();
      stretch_distance = dists.top ();
    }
  else if (gh_car  (symbol_list) == ly_symbol2scm ("minimum-space"))
    {
      spring_len = spring_len >? dists.top ();
      stretch_distance = spring_len;
    }

  /*
    Hint the spacing engine how much space to put in.


    The pairs are in the format of an interval (ie. CAR <  CDR).
  */
  column_l ()->set_elt_property ("extra-space",
				 scm_cons (gh_double2scm (pre_space),
					   gh_double2scm (spring_len)));

  column_l ()->set_elt_property ("stretch-distance",
				 gh_cons (gh_double2scm (-dists[0]),
					  gh_double2scm (stretch_distance)));
				 
}

Break_align_item::Break_align_item ()
{
  stacking_dir_ = RIGHT;
  set_axis (X_AXIS);
}
