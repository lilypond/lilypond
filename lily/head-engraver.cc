/*
  head-grav.cc -- part of GNU LilyPond

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "note-head.hh"
#include "head-engraver.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "dots.hh"

Note_head_engraver::Note_head_engraver()
{
  dot_p_=0;
  note_p_ = 0;
  note_req_l_ =0;
}

bool
Note_head_engraver::do_try_request (Request *req_l) 
{
  if (note_req_l_)
    return false;
  
  if (!(req_l->access_Musical_req () && req_l->access_Musical_req ()->access_Note_req ()))

    return false;
  
  note_req_l_=req_l->access_Musical_req ()->access_Rhythmic_req ();
  return true;
}

void
Note_head_engraver::do_process_requests()
{
  if (!note_req_l_ || note_p_)
    return;
  
  note_p_  = new Note_head;
  note_p_->balltype_i_ = note_req_l_->duration_.durlog_i_;
  note_p_->dots_i_ = note_req_l_->duration_.dots_i_;
  if (note_p_->dots_i_)
    {
      dot_p_ = new Dots;
      note_p_->dots_l_ = dot_p_;
      announce_element (Score_element_info (dot_p_,0));
    }
  
  note_p_->position_i_ = note_req_l_->access_Note_req ()->pitch_.steps ();

  
  Score_element_info itinf (note_p_,note_req_l_);
  announce_element (itinf);
}
 
void
Note_head_engraver::do_pre_move_processing()
{
  if (note_p_) 
    {
      typeset_element (note_p_);
      note_p_ = 0;
    }
  if (dot_p_)
    {
      typeset_element (dot_p_);
      dot_p_ =0;
    }
}
void
Note_head_engraver::do_post_move_processing()
{
  note_req_l_ = 0;
}


IMPLEMENT_IS_TYPE_B1(Note_head_engraver,Engraver);
ADD_THIS_TRANSLATOR(Note_head_engraver);
