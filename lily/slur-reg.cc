/*
  slur-reg.cc -- implement Slur_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "musical-request.hh"
#include "slur-reg.hh"
#include "slur.hh"
#include "debug.hh"
#include "note-column.hh"

bool
Slur_register::do_try_request(Request *req_l)
{
    Musical_req *mus_l = req_l->musical();
    if(!mus_l || !mus_l->slur())
	return false;

    new_slur_req_l_arr_.push(mus_l->slur());
    return true;
}

void
Slur_register::acknowledge_element(Score_elem_info info)
{
    if (info.elem_l_->name() == Note_column::static_name()) { 
	Note_column *col_l =(Note_column*) info.elem_l_->item() ;// ugh
	for (int i = 0; i < slur_l_stack_.size(); i++)
	    slur_l_stack_[i]->add(col_l );
	for (int i = 0; i < end_slur_l_arr_.size(); i++)
	    end_slur_l_arr_[i]->add(col_l);
    }
}

void
Slur_register::set_feature(Feature f)
{
    if (f.type_ == "vdir")
	dir_i_ = f.value_ ;
}

/*
  abracadabra
  */
Slur_register::Slur_register()
{
    dir_i_ =0;
}
void
Slur_register::do_process_requests()
{
    Array<Slur*> start_slur_l_arr_;
    for (int i=0; i< new_slur_req_l_arr_.size(); i++) {
	Slur_req* slur_req_l = new_slur_req_l_arr_[i];
	// end slur: move the slur to other array
	if (slur_req_l->spantype == Span_req::STOP) {
	    if (slur_l_stack_.empty())
		
		      slur_req_l->warning("can't find slur to end");
	    else {
		end_slur_l_arr_.push(slur_l_stack_.pop());
		requests_arr_.pop();
	    }
	} else  if (slur_req_l->spantype == Span_req::START) {
	    // push a new slur onto stack.
	    //(use temp. array to wait for all slur STOPs)
	    Slur * s_p =new Slur;
	    requests_arr_.push(slur_req_l);
	    start_slur_l_arr_.push(s_p);
	    announce_element(Score_elem_info(s_p, slur_req_l));
	}
    }
    for (int i=0; i < start_slur_l_arr_.size(); i++)
	slur_l_stack_.push(start_slur_l_arr_[i]);
}

void
Slur_register::do_pre_move_processing()
{
    for (int i = 0; i < end_slur_l_arr_.size(); i++) {
	if (dir_i_)
	    end_slur_l_arr_[i]->dir_i_ = dir_i_;
	typeset_element(end_slur_l_arr_[i]);
    }
    end_slur_l_arr_.set_size(0);
}

void
Slur_register::do_post_move_processing()
{
    new_slur_req_l_arr_.set_size(0);
}
Slur_register::~Slur_register()
{
    for (int i=0; i < requests_arr_.size(); i++) {
	requests_arr_[i]->warning("unterminated slur");
    }
}
IMPLEMENT_STATIC_NAME(Slur_register);
IMPLEMENT_IS_TYPE_B1(Slur_register,Request_register);
ADD_THIS_REGISTER(Slur_register);
