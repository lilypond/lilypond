/*   
  auto-change-iterator.cc -- implement  Auto_change_iterator

  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "auto-change-music.hh"
#include "auto-change-iterator.hh"
#include "translator-group.hh"
#include "musical-request.hh"



void
Auto_change_iterator::change_to (Music_iterator *it, String to_type,
				 String to_id)
{
  Translator_group * current = it->report_to_l ();
  Translator_group * last = 0;

  /*
    Cut & Paste from Change_iterator (ugh).

    TODO: abstract this function 
   */
  
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
	  it->report_to_l ()->find_create_translator_l (to_type, to_id);
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
	//	error (_ ("I'm one myself"));
      }
  else
    ; //    error (_ ("none of these in my family"));

}

void
Auto_change_iterator::do_process_and_next (Moment m)
{
  Music_wrapper_iterator::do_process_and_next (m);
  Pitch_interrogate_req spanish_inquisition; // nobody expects it

  Music_iterator *it = try_music (&spanish_inquisition);

  if (it && spanish_inquisition.pitch_arr_.size ())
    {
      Musical_pitch p = spanish_inquisition.pitch_arr_[0];
      Direction s = Direction (sign(p.steps ()));
      if (s != where_dir_)
	{
	  where_dir_ = s;
	  String to_id =  (s >= 0) ?  "up" : "down";
	  Auto_change_music const * auto_mus = dynamic_cast<Auto_change_music const* > (music_l_);

	  change_to (it, auto_mus->what_str_, to_id);	  
	}
    }
}

Auto_change_iterator::Auto_change_iterator( )
{
  where_dir_ = CENTER;
}
