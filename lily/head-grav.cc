/*
  headreg.cc -- part of GNU LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "note-head.hh"
#include "head-grav.hh"
#include "paper-def.hh"
#include "musical-request.hh"

Note_head_engraver::Note_head_engraver()
{
    note_p_ = 0;
    do_post_move_processing();
}

bool
Note_head_engraver::do_try_request(Request *req_l) 
{
    if (note_req_l_)
	return false;
    
    if (req_l->musical() && (req_l->musical()->note() || req_l->musical()->rest()))
	note_req_l_=req_l->musical()->rhythmic();
    else
	return false;

    return true;
}

void
Note_head_engraver::do_process_requests()
{
    if (!note_req_l_)
	return;
    
    Note_head*n_p = new Note_head(8);	// ugh
    note_p_ = n_p;
    n_p->set_rhythmic(note_req_l_->rhythmic());

    if (note_req_l_->note()) {
	n_p->position_i_ = note_req_l_->note()->height() +
	    *get_staff_info().c0_position_i_l_;
    } else if (note_req_l_->rest()) {
	n_p->rest_b_ = true;
    }
    
    Score_elem_info itinf(note_p_,note_req_l_);
    announce_element(itinf);
}
 
void
Note_head_engraver::do_pre_move_processing()
{
    if (note_p_) {
	typeset_element(note_p_);
	note_p_ = 0;
    }
}
void
Note_head_engraver::do_post_move_processing()
{
    note_req_l_ = 0;
}

IMPLEMENT_STATIC_NAME(Note_head_engraver);
IMPLEMENT_IS_TYPE_B1(Note_head_engraver,Engraver);
ADD_THIS_ENGRAVER(Note_head_engraver);
