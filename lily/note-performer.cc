/*
  note-performer.cc -- implement Note_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "note-performer.hh"
#include "musical-request.hh"
#include "audio-item.hh"
#include "debug.hh"

IMPLEMENT_IS_TYPE_B1 (Note_performer,Performer);
ADD_THIS_TRANSLATOR (Note_performer);

Note_performer::Note_performer ()
{
  note_req_l_ = 0;
}

void 
Note_performer::do_print () const
{
#ifndef NPRINT
  if (note_req_l_) 
      note_req_l_->print ();
#endif
}

void 
Note_performer::do_process_requests () 
{
  // this is _really_ braindead, but it generates some output
  if (!note_req_l_ || !dynamic_cast <Melodic_req *> (note_req_l_)  || !dynamic_cast <Rhythmic_req *> (note_req_l_))
    return;

  int transposing_i = 0;
  //urg
  Scalar prop = get_property ("transposing");
  if (!prop.empty_b () && prop.isnum_b ()) 
    transposing_i = prop;


  play (new Audio_note (note_req_l_, transposing_i));

  note_req_l_ = 0;
}

bool 
Note_performer::do_try_request (Request* req_l)
{
  if (note_req_l_)
    return false;
  
  if (Note_req *nr = dynamic_cast <Note_req *> (req_l))
    {
      note_req_l_ = nr;
      return true;
    }
  return false;
}
