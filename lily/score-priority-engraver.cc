/*
  score-align-reg.cc -- implement Score_priority_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dimension-cache.hh"
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

/*
  TODO: move this logic into Break_align_item
 */
void
Score_priority_engraver::acknowledge_element (Score_element_info inf)
{
  if (Item * item_l = dynamic_cast <Item *> (inf.elem_l_))
    {
      if (item_l->empty_b (X_AXIS) || item_l->parent_l (X_AXIS))
	return;

      SCM bp=item_l->remove_elt_property ("breakable");
      bool breakable = (bp != SCM_UNDEFINED);
      if (!breakable)
	return ;


      if (!halign_p_)
	{
	  halign_p_ = new Break_align_item;
	  halign_p_->set_elt_property ("breakable", SCM_BOOL_T);
	  announce_element (Score_element_info (halign_p_,0));
	}

      halign_p_->add_breakable_item (item_l);
    }
}

ADD_THIS_TRANSLATOR(Score_priority_engraver);

