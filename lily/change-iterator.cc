/*
  change-iterator.cc -- implement Change_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "change-iterator.hh"
#include "translator-group.hh"
#include "change-translator.hh"
#include "debug.hh"


void
Change_iterator::error (String reason)
{
  String to_type = change_l ()->change_to_type_str_;
  String to_id =  change_l ()->change_to_id_str_;

  String warn1 = _f ("can't change `%s\' to `%s\'", to_type, to_id) 
    + ": " + reason;
  String warn2= "Change_iterator::do_process_and_next (): " 
    + report_to_l ()->type_str_ + " = `"
    + report_to_l ()->id_str_ + "\': ";
  warning (warn2);
  change_l ()->warning (warn1);
}

Change_translator *
Change_iterator::change_l ()
{
  return (Change_translator*) music_l_;
}

/*
  move to construct_children ?
 */
void
Change_iterator::do_process_and_next (Moment m)
{
  Translator_group * current = report_to_l ();
  Translator_group * last = 0;

  String to_type = change_l ()->change_to_type_str_;
  String to_id =  change_l ()->change_to_id_str_;

  /* find the type  of translator that we're changing.
     
     If \translator Staff = bass, then look for Staff = *
   */
  while  (current && current->type_str_ != to_type)
    {
      last = current;
      current = current->daddy_trans_l_;
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
	error ("I'm one myself");
      }
  else
    error ("None of these in my family.");
  Music_iterator::do_process_and_next (m);
}

IMPLEMENT_IS_TYPE_B1 (Change_iterator, Music_iterator);
