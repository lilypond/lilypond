/*
  tie-reg.cc -- implement Tie_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "tie-reg.hh"
#include "tie.hh"
#include "notehead.hh"
#include "musical-request.hh"
#include "voice-element.hh"

Tie_register::Tie_register()
{
    end_tie_p_ = 0;
    tie_p_ = 0;
    req_l_ =0;
    end_req_l_ =0;
    end_mom_ = -1;
}

void
Tie_register::post_move_processing()
{
     if (tie_p_ && get_staff_info().when() == end_mom_) {
	end_tie_p_ = tie_p_;
	end_req_l_ = req_l_;
	tie_p_ =0;
	req_l_ =0;
	end_mom_ = -1;
    }
}

bool
Tie_register::acceptable_request_b(Request*r)
{
    return r->musical() && r->musical()->tie();
}

bool
Tie_register::try_request(Request*r)
{
    if(!acceptable_request_b(r))
	return false;
    if (req_l_ ) {
	return false;
    }
    req_l_ = r->musical()->tie();
    end_mom_ = r->elt_l_->duration_ + get_staff_info().when();
    return true;
}

void
Tie_register::process_requests()
{
    if (req_l_ && ! tie_p_) {
	tie_p_ = new Tie;
    }
}

void
Tie_register::acknowledge_element(Staff_elem_info i)
{
    if (i.elem_l_->name() == Notehead::static_name()) {
	if (tie_p_)
	    tie_p_->set_head(-1, (Notehead*)i.elem_l_);
	
	if (end_tie_p_) {
	    end_tie_p_->set_head(1, (Notehead*)i.elem_l_);
	    announce_element(Staff_elem_info(end_tie_p_,end_req_l_));
	}
    }
}

void
Tie_register::pre_move_processing()
{
    if (end_tie_p_) {
	typeset_element(end_tie_p_);
	end_tie_p_ =0;
	end_req_l_ =0;
    }
 
}

Tie_register::~Tie_register()
{
    if (tie_p_) {
	req_l_->warning("unended Tie");
	delete tie_p_;
    }
}

IMPLEMENT_STATIC_NAME(Tie_register);
ADD_THIS_REGISTER(Tie_register);
