/*
  change-iterator.cc -- implement Change_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
    + get_outlet ()->context_name () + " = `"
    + get_outlet ()->id_string_ + "': ";
  warning (warn2);
  get_music ()->origin ()->warning (warn1);
}

/*
  move to construct_children ?
 */
void
Change_iterator::process (Moment m)
{
  Translator_group * current = get_outlet ();
  Translator_group * last = 0;

  SCM to_type = get_music ()->get_mus_property ("change-to-type");
  String to_id =  ly_scm2string (get_music ()->get_mus_property ("change-to-id"));


  /* find the type  of translator that we're changing.
     
     If \translator Staff = bass, then look for Staff = *
   */
  while (current && !current->is_alias (to_type))
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
	  get_outlet ()->find_create_translator (to_type, to_id, SCM_EOL);
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
