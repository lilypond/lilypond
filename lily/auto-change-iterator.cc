/*   
  auto-change-iterator.cc -- implement  Auto_change_iterator

  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
	dest->add_group_translator (last);
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

/*
  Look ahead to find first pitches to determine staff position.
  WARNING: this means that

  \autochange Staff \notes { .... \context Staff = otherstaff { .. } .. }

  will confuse the autochanger, since it will not notice that the
  music for OTHERSTAFF is not his.

  PRECONDITION: this->ok() holds.
*/
Array<Pitch>
Auto_change_iterator::pending_pitch (Moment m) const
{
  Music_iterator * iter = child_iter_p_ ->clone ();
  Array<Pitch> ps;
  while (1)
    {
      SCM muses = iter->get_music (m);
      for (SCM s = muses; gh_pair_p (s); s=gh_cdr (s))
	if (Note_req* nr = dynamic_cast<Note_req*> (unsmob_music (gh_car (s))))
	  {
	    ps.push (*unsmob_pitch (nr->get_mus_property ("pitch")));
	  }

      if (ps.size ())
	break;

      iter->skip (m);
      if (!iter->ok())
	break;
      
      m = iter->pending_moment ();
    }

  delete iter;
  return ps;
}

void
Auto_change_iterator::process (Moment m)
{
  /*
    first we get the pitches, then we do the real work.
    Music_wrapper_iterator::process() might process (and throw away)
    pitches we need.  */
  Array<Pitch> ps = pending_pitch (m);

  Music_wrapper_iterator::process (m);
  if (ps.size ())
    {
      Pitch p = ps[0];
      Direction s = Direction (sign(p.steps ()));
      if (s != where_dir_)
	{
	  where_dir_ = s;
	  String to_id =  (s >= 0) ?  "up" : "down";
	  Auto_change_music const * auto_mus = dynamic_cast<Auto_change_music const* > (music_l_);

	  String wh = ly_scm2string (auto_mus->get_mus_property ("what"));
	  change_to (child_iter_p_, wh, to_id);	  
	}
    }
}

Auto_change_iterator::Auto_change_iterator( )
{
  where_dir_ = CENTER;
}

IMPLEMENT_CTOR_CALLBACK(Auto_change_iterator);
