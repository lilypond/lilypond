/*
  headreg.cc -- part of GNU LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "note-head.hh"
#include "head-reg.hh"
#include "paper-def.hh"
#include "complex-walker.hh"
#include "musical-request.hh"

Note_head_register::Note_head_register()
{
    note_p_ = 0;
    post_move_processing();
}

bool
Note_head_register::try_request(Request *req_l) 
{
    if (req_l->note() || req_l->rest())
	note_req_l_=req_l->rhythmic();
    else
	return false;

    return true;
}

void
Note_head_register::process_requests()
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
	if (note_req_l_->rhythmic()->duration_.type_i_ <= 2)
	    note_p_->translate(
		Offset(0,
		       6 * paper()->internote_f()));
    }
    
    Score_elem_info itinf(note_p_,note_req_l_);
    announce_element(itinf);
}

void
Note_head_register::pre_move_processing()
{
    if (note_p_) {
	typeset_element(note_p_);
	note_p_ = 0;
    }
}
void
Note_head_register::post_move_processing()
{
    note_req_l_ = 0;
}

IMPLEMENT_STATIC_NAME(Note_head_register);
ADD_THIS_REGISTER(Note_head_register);
