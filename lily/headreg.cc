/*
  headreg.cc -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "rest.hh"
#include "notehead.hh"
#include "headreg.hh"
#include "paper-def.hh"
#include "complex-walker.hh"
#include "musical-request.hh"

Notehead_register::Notehead_register()
{
    note_p_ = 0;
    set_feature(Features::dir(0));
    post_move_processing();
}

bool
Notehead_register::try_request(Request *req_l) 
{
    if (req_l->note() || req_l->rest())
	note_req_l_=req_l->rhythmic();
    else
	return false;

    return true;
}
void
Notehead_register::set_feature(Features d)
{
    if(d.direction_i_ || d.initialiser_b_)
	dir_i_ = d.direction_i_;
}

void
Notehead_register::process_requests()
{
    if (!note_req_l_)
	return;
    

    if (note_req_l_->note()) {
	Notehead*n_p = new Notehead(8);	// ugh
	note_p_ = n_p;
	n_p->set_rhythmic(note_req_l_->rhythmic());
	n_p->position = note_req_l_->note()->height() +
	    *get_staff_info().c0_position_i_;
    } else {
	note_p_ = new Rest ( note_req_l_->rhythmic()->duration_);
	if (note_req_l_->rhythmic()->duration_.type_i_ <= 2)
	    note_p_->translate(
		Offset(0,
		       6 * paper()->internote()));
    }
    Staff_elem_info itinf(note_p_,note_req_l_);
    announce_element(itinf);
}

void
Notehead_register::pre_move_processing()
{
    if (note_p_) {
	if (dir_i_ && note_p_->name() == Rest::static_name())
	    note_p_->translate(Offset(0, 4*dir_i_ * paper()->internote()));
	typeset_element(note_p_);
	note_p_ = 0;
    }
}
void
Notehead_register::post_move_processing()
{
    note_req_l_ = 0;
}

IMPLEMENT_STATIC_NAME(Notehead_register);
