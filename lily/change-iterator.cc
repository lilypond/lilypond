/*
  change-iterator.cc -- implement 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "change-iterator.hh"
#include "translator-group.hh"
#include "change-translator.hh"

Change_iterator::Change_iterator (Change_translator *change_l)
{
  change_l_ = change_l;
}

/*
  move to construct_children ?
 */
void
Change_iterator::process_and_next (Moment m)
{
  Translator_group * current = report_to_l ();
  Translator_group * last = 0;
  while  (current && current->type_str_ != change_l_->change_to_type_str_)
    {
      last = current;
      current = current->daddy_trans_l_;
    }
  if (current) 
    {
      Translator_group * dest = 
	report_to_l ()->find_create_translator_l (change_l_->change_to_type_str_, 
						  change_l_->change_to_id_str_);
      current->remove_translator_p (last);
      dest->add (last);
    }
  
  Music_iterator::process_and_next (m);
}

IMPLEMENT_IS_TYPE_B1 (Change_iterator, Music_iterator);
