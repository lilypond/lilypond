/*
  score-align-reg.cc -- implement Score_priority_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "axis-group-item.hh"
#include "score-priority-engraver.hh"
#include "item.hh"
#include "dictionary-iter.hh"
#include "break-align-item.hh"


Score_priority_engraver::Score_priority_engraver()
{
  halign_p_ = 0;
}

void
Score_priority_engraver::do_pre_move_processing()
{
  for (int i=0; i < column_p_arr_.size ();i++)
    typeset_element (column_p_arr_[i]);
  column_p_arr_.clear ();

  if (halign_p_)
    {
      typeset_element (halign_p_);
      halign_p_ =0;
    }
  
}

void
Score_priority_engraver::add_horizontal_group (Item* it, int priority)
{
  if (!halign_p_)
    {
      halign_p_ = new Break_align_item;
      halign_p_->set_elt_property (breakable_scm_sym, SCM_BOOL_T);
      announce_element (Score_element_info (halign_p_,0));
    }

  if (priority == 0)
    halign_p_->center_l_ = it;

  halign_p_->add_element_priority (it, priority);

  column_p_arr_.push (it);
}

void
Score_priority_engraver::acknowledge_element (Score_element_info inf)
{
  if (Item * item_l = dynamic_cast <Item *> (inf.elem_l_))
    {
      Dimension_cache * c = item_l->dim_cache_[X_AXIS];
      if (c->empty_b () || c->parent_l_)
	return;

      SCM pr = item_l->remove_elt_property (break_priority_scm_sym); 

      if (pr == SCM_BOOL_F)
	return;

      bool breakable = (item_l->remove_elt_property (breakable_scm_sym) != SCM_BOOL_F);
      if (!breakable)
	return ;
      
      int priority = gh_scm2int (SCM_CDR (pr));
      
      Score_element * column_l = 0;
      if (halign_p_)
	column_l = halign_p_->get_elt_by_priority (priority);
      Axis_group_item * hg=0;
      if (column_l)
	{
	  hg = dynamic_cast<Axis_group_item*> (column_l);
	}
      else
	{
	  hg = new Axis_group_item;
	  hg->set_axes (X_AXIS,X_AXIS);
	  hg->set_elt_property (ly_symbol("origin"),
				SCM_EOL);
	  announce_element (Score_element_info (hg,0));
	  add_horizontal_group (hg, priority);
	}
      
      hg->set_elt_property (ly_symbol("origin"),
			    scm_cons (gh_str02scm (item_l->name()),
				      hg->get_elt_property (ly_symbol ("origin"))));
      hg->add_element (item_l);
      
    }
}

ADD_THIS_TRANSLATOR(Score_priority_engraver);
