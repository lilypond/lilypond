/*
  score-align-reg.cc -- implement Score_priority_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "horizontal-group-item.hh"
#include "score-priority-engraver.hh"
#include "item.hh"
#include "assoc-iter.hh"
#include "break-align-item.hh"


Score_priority_engraver::Score_priority_engraver()
{
}

void
Score_priority_engraver::do_pre_move_processing()
{
  for (Assoc_iter<int, Horizontal_group_item*> i(align_p_assoc_); i.ok() ; i++)
    {
      if (i.val ())
	{
	  typeset_element (i.val ());
	  i.val () = 0;
	}
    }
  align_p_assoc_.clear ();
}

void
Score_priority_engraver::acknowledge_element (Score_element_info inf)
{
  Item * item_l = inf.elem_l_->access_Item ();
  if (item_l && item_l->breakable_b_ && !item_l->empty_b ())
    {
      /*
	Don't try to eat up our (probable) parent.
      */
      if (inf.origin_grav_l_arr_.size () <= 1 &&
	  item_l->is_type_b (Break_align_item::static_name ()))
	return; 

      
      int priority =item_l->break_priority_i_;
      Horizontal_group_item * hg =0;
      if (!align_p_assoc_.elem_b(priority))
	{
	  hg = new Horizontal_group_item;
	  announce_element (Score_element_info (hg,0));
	  align_p_assoc_[priority] = hg;
	  hg->break_priority_i_ = priority;
	  hg->breakable_b_ = true;
	}
      else
	hg = align_p_assoc_[priority];
      
      Score_element * unbound_elem = inf.elem_l_;

      while (unbound_elem->axis_group_l_a_[X_AXIS])
	{
	  /* We might have added inf.elem_l_ earlier because we added one
	     of its children.  We don't want to add ourselves to ourself
	  */
	  if (unbound_elem->axis_group_l_a_[X_AXIS] == hg)
	    return;
	  unbound_elem = unbound_elem->axis_group_l_a_[X_AXIS]->access_Score_element ();
	}

      hg->add_element (unbound_elem);
    }
}

IMPLEMENT_IS_TYPE_B1(Score_priority_engraver, Engraver);
ADD_THIS_TRANSLATOR(Score_priority_engraver);
