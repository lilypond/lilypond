/*   
  dot-column-engraver.cc -- implement Dot_column_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "dot-column-engraver.hh"
#include "rhythmic-head.hh"
#include "dot-column.hh"

Dot_column_engraver::Dot_column_engraver ()
{
  dotcol_p_ =0;
}

void
Dot_column_engraver::do_pre_move_processing ()
{
  if (dotcol_p_)
    {
      typeset_element (dotcol_p_);
      dotcol_p_ =0;
    }
  head_l_arr_.clear ();
}

void
Dot_column_engraver::acknowledge_element (Score_element_info info)
{
  Item * i=dynamic_cast <Item *> (info.elem_l_);
  
  if (! (i && i->is_type_b (Rhythmic_head::static_name ())))
      return;

  Rhythmic_head * h = (Rhythmic_head*)i;
  
  if (!h->dots_l_)
    return;

  if (!dotcol_p_)
    {
      dotcol_p_ = new Dot_column;
      announce_element (Score_element_info (dotcol_p_, 0));
    }

  dotcol_p_->add_head (h);
}


ADD_THIS_TRANSLATOR(Dot_column_engraver);
IMPLEMENT_IS_TYPE_B1(Dot_column_engraver,Engraver);
