/*
  change-iterator.cc -- implement Change_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "change-iterator.hh"
#include "translator-group.hh"
#include "music.hh"
#include "warn.hh"



void
Change_iterator::error (String reason)
{
  String to_type = ly_scm2string (get_music ()->get_mus_property ("change-to-type"));
  String to_id =  ly_scm2string (get_music ()->get_mus_property ("change-to-id"));

  String warn1 = _f ("can't change `%s' to `%s'", to_type, to_id) 
    + ": " + reason;
  /*
    GUHG!
   */
  String warn2= "Change_iterator::process (): " 
    + report_to ()->type_string_ + " = `"
    + report_to ()->id_string_ + "': ";
  warning (warn2);
  get_music ()->origin ()->warning (warn1);
}

/*
  move to construct_children ?
 */
void
Change_iterator::process (Moment m)
{
  Translator_group * current = report_to ();
  Translator_group * last = 0;

  String to_type = ly_scm2string (get_music ()->get_mus_property ("change-to-type"));
  String to_id =  ly_scm2string (get_music ()->get_mus_property ("change-to-id"));


  /* find the type  of translator that we're changing.
     
     If \translator Staff = bass, then look for Staff = *
   */
  while (current && current->type_string_ != to_type)
    {
      last = current;
      current = current->daddy_trans_;
    }

  if (current && current->id_string_ == to_id)
    {
      String msg;
      msg += _ ("Can't switch translators, I'm there already");
    }
  
  if (current) 
    if (last)
      {
	Translator_group * dest = 
	  report_to ()->find_create_translator (to_type, to_id);
	current->remove_translator (last);
	dest->add_used_group_translator (last);
      }
    else
      {
	/*
	  We could change the current translator's id, but that would make 
	  errors hard to catch
	  
	   last->translator_id_string_  = get_change ()->change_to_id_string_;
	*/
	error (_ ("I'm one myself"));
      }
  else
    error (_ ("none of these in my family"));
  Simple_music_iterator::process (m);
}



IMPLEMENT_CTOR_CALLBACK (Change_iterator);
