/*
  change-iterator.cc -- implement Change_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "change-iterator.hh"
#include "translator-group.hh"
#include "change-translator.hh"
#include "debug.hh"


void
Change_iterator::error (String reason)
{
  Change_translator const * t = dynamic_cast<Change_translator const*> (music_l_);   
  String to_type = t->change_to_type_str_;
  String to_id =  t->change_to_id_str_;

  String warn1 = _f ("Can't change `%s' to `%s'", to_type, to_id) 
    + ": " + reason;
  /*
    GUHG!
   */
  String warn2= "Change_iterator::do_process_and_next (): " 
    + report_to_l ()->type_str_ + " = `"
    + report_to_l ()->id_str_ + "': ";
  warning (warn2);
  t->warning (warn1);
}

/*
  move to construct_children ?
 */
void
Change_iterator::do_process_and_next (Moment m)
{
  Translator_group * current = report_to_l ();
  Translator_group * last = 0;

  Change_translator const * t = dynamic_cast<Change_translator const*> (music_l_); 
  String to_type = t->change_to_type_str_;
  String to_id =  t->change_to_id_str_;

  /* find the type  of translator that we're changing.
     
     If \translator Staff = bass, then look for Staff = *
   */
  while  (current && current->type_str_ != to_type)
    {
      last = current;
      current = current->daddy_trans_l_;
    }

  if (current && current->id_str_ == to_id)
    {
      String msg;
      msg += _ ("Can't switch translators, I'm there already");
    }
  
  if (current) 
    if (last)
      {
	Translator_group * dest = 
	  report_to_l ()->find_create_translator_l (to_type, to_id);
	current->remove_translator_p (last);
	dest->add_translator (last);
      }
    else
      {
	/*
	  We could change the current translator's id, but that would make 
	  errors hard to catch
	  
	   last->translator_id_str_  = change_l ()->change_to_id_str_;
	*/
	error (_ ("I'm one myself"));
      }
  else
    error (_ ("none of these in my family"));
  Music_iterator::do_process_and_next (m);
}


