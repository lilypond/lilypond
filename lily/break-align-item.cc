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
  sort_elements ();
  Real interline= paper_l ()->get_realvar (interline_scm_sym);	
  
  Link_array<Score_element> elems;
  for (int i=0; i < elem_l_arr_.size(); i++) 
    {
      Interval y = elem_l_arr_[i]->extent(axis ());
      if (!y.empty_b())
	elems.push (dynamic_cast<Score_element*> (elem_l_arr_[i]));
    }
  
  if (!elems.size ())
    return;

  SCM symbol_list = SCM_EOL;
  Array<Real> dists;
  SCM current_origin = ly_ch_C_to_scm ("");
  for (int i=0; i <= elems.size (); i++)
    {
      Score_element *next_elt  = i < elems.size ()
	? elems[i]
	: 0 ;
      
      SCM next_origin;

      if (next_elt)
	{
	  next_origin = next_elt->get_elt_property (origin_scm_sym);
	  next_origin =
	    (next_origin == SCM_BOOL_F)
	    ? ly_ch_C_to_scm ("")
	    : SCM_CDR (next_origin);
	}
      else
	next_origin = ly_ch_C_to_scm ("begin-of-note");
      
      SCM extra_space
	= scm_eval (scm_listify (ly_symbol ("break-align-spacer"),
				 current_origin, next_origin, SCM_UNDEFINED)); 
      SCM symbol = SCM_CAR (extra_space);
      Real spc = gh_scm2double (SCM_CADR(extra_space));
      spc *= interline;

      dists.push(spc);
      symbol_list = gh_cons (symbol, symbol_list);
      current_origin = next_origin;
    }


  // skip the first sym.
  symbol_list  = SCM_CDR (scm_reverse (symbol_list));
  for (int i=0; i <elems.size()-1; i++)
    {
      elems[i]->set_elt_property (SCM_CAR (symbol_list),
				  scm_cons (gh_double2scm (0),
					    gh_double2scm (dists[i+1])));

      symbol_list = SCM_CDR (symbol_list);
    }


  // urg
  SCM first_pair = elems[0]->get_elt_property (minimum_space_scm_sym);
  if (first_pair == SCM_BOOL_F)
    first_pair = gh_cons (gh_double2scm (0.0), gh_double2scm (0.0));
  else
    first_pair = SCM_CDR (first_pair);
  
  scm_set_car_x (first_pair, gh_double2scm (-dists[0]));
  elems[0]->set_elt_property (minimum_space_scm_sym, first_pair);
  
  Axis_align_item::do_pre_processing();


  Real pre_space = elems[0]->extent (X_AXIS)[LEFT]
    + elems[0]->relative_coordinate (column_l (), X_AXIS);
  Real spring_len = elems.top ()->extent (X_AXIS)[RIGHT]
    + elems.top ()->relative_coordinate (column_l (), X_AXIS);
  
  Real stretch_distance =0.;
  
  if (SCM_CAR (symbol_list) == extra_space_scm_sym)
    {
      spring_len += dists.top ();
      stretch_distance = dists.top ();
    }
  else if (SCM_CAR (symbol_list) == minimum_space_scm_sym)
    {
      spring_len = spring_len >? dists.top ();
      stretch_distance = spring_len;
    }

  /*
    Hint the spacing engine how much space to put in.


    The pairs are in the format of an interval (ie. CAR <  CDR).
  */
  column_l ()->set_elt_property (extra_space_scm_sym,
				 scm_cons (gh_double2scm (pre_space),
					   gh_double2scm (spring_len)));

  column_l ()->set_elt_property (stretch_distance_scm_sym,
				 gh_cons (gh_double2scm (-dists[0]),
					  gh_double2scm (stretch_distance)));
				 
}

Break_align_item::Break_align_item ()
{
  stacking_dir_ = RIGHT;
  set_axis (X_AXIS);
}

void
Break_align_item::add_breakable_item (Item *it)
{
  SCM pr = it->remove_elt_property (break_priority_scm_sym); 

  if (pr == SCM_BOOL_F)
    return;

  int priority = gh_scm2int (SCM_CDR (pr));

  Score_element * column_l = get_elt_by_priority (priority);
  Axis_group_item * hg=0;
  if (column_l)
    {
      hg = dynamic_cast<Axis_group_item*> (column_l);
    }
  else
    {
      hg = new Axis_group_item;
      hg->set_axes (X_AXIS,X_AXIS);

      /*
	this is quite ridiculous, but we do this anyway, to ensure that no
	warning bells about missing Y refpoints go off later on.
      */
      hg->set_parent (this, Y_AXIS);
      hg->set_elt_property (ly_symbol ("origin"), ly_ch_C_to_scm (it->name ()));

      pscore_l_->typeset_element (hg);
      add_element_priority (hg, priority);

      if (priority == 0)
	center_l_ = hg;
    }
  
  hg->add_element (it);
}
