/*
  tie-reg.cc -- implement Tie_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "tie-reg.hh"
#include "tie.hh"
#include "note-head.hh"
#include "musical-request.hh"
#include "music-list.hh"

Tie_register::Tie_register()
{
    end_tie_p_ = 0;
    tie_p_ = 0;
    req_l_ =0;
    end_req_l_ =0;
    end_mom_ = -1;
    melodic_req_l_ = 0;
    end_melodic_req_l_ =0;
}

void
Tie_register::sync_features()
{
    dir_i_ = get_feature("vdir");
}
    

void
Tie_register::do_post_move_processing()
{
    if (tie_p_ && get_staff_info().when() == end_mom_) {
	end_tie_p_ = tie_p_;
	end_req_l_ = req_l_;
	end_melodic_req_l_ = melodic_req_l_;
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
Tie_register::do_try_request(Request*r)
{
    if(!acceptable_request_b(r))
	return false;
    if (req_l_ ) {
	return false;
    }
    req_l_ = r->musical()->tie();
    end_mom_ = r->parent_music_l_->time_int().length()
	+ get_staff_info().when();
    return true;
}

void
Tie_register::do_process_requests()
{
    if (req_l_ && ! tie_p_) {
	tie_p_ = new Tie;
    }
}

void
Tie_register::acknowledge_element(Score_elem_info i)
{
    if (i.elem_l_->name() == Note_head::static_name()) {
	if (tie_p_) {
	    tie_p_->set_head(-1, (Note_head*)i.elem_l_->item());
	    melodic_req_l_ = i.req_l_->musical()->melodic();
	}

	if (end_tie_p_) {
	    end_tie_p_->set_head(1, (Note_head*)i.elem_l_->item());
	    if (!Melodic_req::compare ( *end_melodic_req_l_, *melodic_req_l_))
		end_tie_p_->same_pitch_b_ = true;
	    announce_element(Score_elem_info(end_tie_p_,end_req_l_));
	}
    }
}

void
Tie_register::do_pre_move_processing()
{
    if (end_tie_p_) {
	if (dir_i_)
	    end_tie_p_->dir_i_ =  dir_i_;
	
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

void
Tie_register::set_feature(Feature f)
{
    if (f.type_ == "vdir")
	dir_i_ = f.value_;
}

IMPLEMENT_STATIC_NAME(Tie_register);
IMPLEMENT_IS_TYPE_B1(Tie_register,Request_register);
ADD_THIS_REGISTER(Tie_register);
