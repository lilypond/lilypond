/*
  break-align-item.cc -- implement Break_align_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "lily-guile.hh"
#include "break-align-item.hh"
#include "dimensions.hh"
#include "p-score.hh"
#include "paper-def.hh"

void
Break_align_item::do_pre_processing()
{
  align_dir_ = break_status_dir();
  flip (&align_dir_);
  sort_elements ();

  
  Link_array<Score_element> elems;
  for (int i=0; i < elem_l_arr_.size(); i++) 
    {
      Interval y = elem_l_arr_[i]->extent(axis ());
      if (!y.empty_b())
	elems.push (dynamic_cast<Score_element*> (elem_l_arr_[i]));
    }
  

  if (elems.size ())
    {
      Score_element *current_elt =elems[0];
      SCM current_origin = current_elt->get_elt_property (origin_scm_sym);

      if (current_origin != SCM_BOOL_F)
	current_origin = SCM_CDR (current_origin);
      
      for (int i=1; i < elems.size (); i++)
	{
	  Score_element *next_elt  = elems[i];
	  SCM next_origin = next_elt->get_elt_property (origin_scm_sym);
	  if (next_origin != SCM_BOOL_F)
	    {
	      next_origin = SCM_CDR(next_origin);
	      SCM extra_space = scm_eval (scm_listify (ly_symbol ("break-align-spacer"),
							    current_origin,
							    next_origin,
							    SCM_UNDEFINED));

	      
	      Real spc = gh_scm2double (extra_space);
	      spc *= paper_l ()->get_realvar (interline_scm_sym);	
	      
	      current_elt->set_elt_property (extra_space_scm_sym,
					     scm_cons (gh_double2scm (0.0),
						       gh_double2scm (spc)));
	      
	    }
	  current_elt = next_elt;
	  current_origin = next_origin;	  
	}

    }
  Axis_align_item::do_pre_processing();
}




Break_align_item::Break_align_item ()
{
  stacking_dir_ = RIGHT;
  threshold_interval_[SMALLER] = 1.5 PT;
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
      hg->dim_cache_[Y_AXIS]->parent_l_ = dim_cache_[Y_AXIS];
      hg->set_elt_property (ly_symbol("origin"), gh_str02scm (it->name()));

      pscore_l_->typeset_element (hg);
      add_element_priority (hg, priority);

      if (priority == 0)
	center_l_ = hg;
    }
  
  /*
  hg->set_elt_property (ly_symbol("origin"),
			scm_cons (gh_str02scm (it->name()),
				  hg->get_elt_property (ly_symbol ("origin"))));
  */
  hg->add_element (it);


}
