/*
  score-align-reg.cc -- implement Score_priority_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "horizontal-group-item.hh"
#include "score-priority-engraver.hh"
#include "item.hh"
#include "dictionary-iter.hh"
#include "break-align-item.hh"


Score_priority_engraver::Score_priority_engraver()
{
  align_p_tab_.hash_func_ = int_hash;
}

void
Score_priority_engraver::do_pre_move_processing()
{
  for (Hash_table_iter<int, Horizontal_group_item*> i(align_p_tab_);
       i.ok() ; i++)
    {
      if (i.val ())
	{
	  typeset_element (i.val ());
	  i.val_ref () = 0;
	}
    }
  align_p_tab_.clear ();
}

void
Score_priority_engraver::acknowledge_element (Score_element_info inf)
{
  Item * item_l = dynamic_cast <Item *> (inf.elem_l_);
  if (item_l && item_l->breakable_b_ && !item_l->empty_b ())
    {
      /*
	Don't try to eat up our (probable) parent.
      */
      if (inf.origin_grav_l_arr_.size () <= 1 &&
	  dynamic_cast<Break_align_item *> (item_l))
	return; 

      
      int priority =item_l->break_priority_i_;
      Horizontal_group_item * hg =0;
      if (!align_p_tab_.elem_b(priority))
	{
	  hg = new Horizontal_group_item;
	  announce_element (Score_element_info (hg,0));
	  align_p_tab_[priority] = hg;
	  hg->break_priority_i_ = priority;
	  hg->breakable_b_ = true;
	}
      else
	hg = align_p_tab_[priority];
      
      Score_element * unbound_elem = inf.elem_l_;

      /*
	ugh
       */
      while (unbound_elem->parent_l (X_AXIS))
	{
	  /* We might have added inf.elem_l_ earlier because we added one
	     of its children.  We don't want to add ourselves to ourself
	  */
	  Graphical_element * e = unbound_elem->parent_l (X_AXIS);
	  if (e == hg)
	    return;
	  unbound_elem = dynamic_cast<Score_element*> (e);
	}

      hg->add_element (unbound_elem);
    }
}


ADD_THIS_TRANSLATOR(Score_priority_engraver);
