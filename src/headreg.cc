/*
  headreg.cc -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "rest.hh"
#include "notehead.hh"
#include "headreg.hh"
#include "paper.hh"
#include "complexwalker.hh"


Notehead_register::Notehead_register(Complex_walker*w_l)
    :Request_register(w_l)
{
    note_p_ = 0;
    set_dir(0);
}

bool
Notehead_register::try_request(Request *req_l) 
{
    if (req_l->note() || req_l->rest())
	accepted_req_arr_.push(req_l);
    else
	return false;

    return true;
}
void
Notehead_register::set_dir(int d)
{
    dir_i_ = d;
}

void
Notehead_register::process_request()
{
    if (!accepted_req_arr_.size())
	return;
    
    Request* req_l = accepted_req_arr_.top();
    if (req_l->note()) {
	Notehead*n_p = new Notehead(8);	// ugh
	note_p_ = n_p;
	n_p->set_rhythmic(req_l->rhythmic());
	n_p->position = req_l->note()->height() +
	    walk_l_->clef_.c0_position_i_;
    } else {
	note_p_ = new Rest ( req_l->rhythmic()->balltype,
			     req_l->rhythmic()->dots);
	if (req_l->rhythmic()->balltype <= 2)
	    note_p_->translate(
		Offset(0,
		       6 * paper()->internote()));
    }
    Staff_elem_info itinf(note_p_,req_l,this);
    announce_element(itinf);
}

void
Notehead_register::do_pre_move_process()
{
    if (note_p_) {
	if (dir_i_ && note_p_->name() == String("Rest"))
	    note_p_->translate(Offset(0, 4*dir_i_ * paper()->internote()));
	typeset_element(note_p_);
	note_p_ = 0;
    }
}
