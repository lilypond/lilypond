/*
  note-performer.cc -- implement Note_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "note-performer.hh"
#include "musical-request.hh"
#include "audio-item.hh"
#include "debug.hh"


ADD_THIS_TRANSLATOR (Note_performer);

Note_performer::Note_performer ()
{
}

void 
Note_performer::do_print () const
{
#ifndef NPRINT
  if (note_req_l_arr_.size()>0)
    for(int i=0;i<note_req_l_arr_.size();i++)
      note_req_l_arr_[i]->print ();
#endif
}

void 
Note_performer::do_process_requests () 
{
  if (note_req_l_arr_.size ())
    {
      int transposing_i = 0;
      //urg
      Scalar prop = get_property ("transposing", 0);
      if (!prop.empty_b () && prop.isnum_b ()) 
	transposing_i = prop;

      while(note_req_l_arr_.size ())
	{
	  Note_req* n = note_req_l_arr_.pop ();
	  Audio_note* p = new Audio_note (n->pitch_, n->length_mom (), transposing_i);
	  Audio_element_info info (p, n);
	  announce_element (info);
	  note_p_arr_.push (p);
	}
    }
}

void
Note_performer::do_pre_move_processing ()
{
  for (int i=0; i < note_p_arr_.size (); i++)
    {
      play_element (note_p_arr_[i]);
    }
  note_p_arr_.clear ();
  note_req_l_arr_.clear ();
}
 
bool
Note_performer::do_try_music (Music* req_l)
{
  if (Note_req *nr = dynamic_cast <Note_req *> (req_l))
    {
      note_req_l_arr_.push (nr);
      return true;
    }
  return false;
}
