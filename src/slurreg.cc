/*
  slurreg.cc -- implement Slur_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "complexwalker.hh"
#include "slurreg.hh"
#include "slur.hh"
#include "debug.hh"
void
Slur_register::set_dir(int i)
{
    dir_i_ = i;
}

Slur_register::Slur_register(Complex_walker* w)
    : Request_register(w)
{
    set_dir(0);
}

bool
Slur_register::try_request(Request *req_l)
{
    if(!req_l->slur())
	return false;

    accepted_req_arr_.push(req_l);
    return true;
}

void
Slur_register::acknowledge_element(Staff_elem_info info)
{
    if (info.elem_p_->name() == String("Notehead")) { 
	Notehead *head_p =(Notehead*) info.elem_p_ ;// ugh
	for (int i = 0; i < slur_l_stack_.size(); i++)
	    slur_l_stack_[i]->add(head_p );
	for (int i = 0; i < end_slur_l_arr_.size(); i++)
	    end_slur_l_arr_[i]->add(head_p);
    }
}
/*
  abracadabra
  */
void
Slur_register::process_request()
{
    Array<Slur*> start_slur_l_arr_;
    for (int i=0; i< accepted_req_arr_.size(); i++) {
	Slur_req* slur_req_l = accepted_req_arr_[i]->slur();
	// end slur: move the slur to other array
	if (slur_req_l->spantype == Span_req::STOP) {
	    if (slur_l_stack_.empty())
		warning("can't find slur to end",
		      slur_req_l->defined_ch_c_l_m);
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
	    announce_element(Staff_elem_info(s_p, slur_req_l, this));
	}
    }
    for (int i=0; i < start_slur_l_arr_.size(); i++)
	slur_l_stack_.push(start_slur_l_arr_[i]);
}

void
Slur_register::do_pre_move_process()
{
    for (int i = 0; i < end_slur_l_arr_.size(); i++) {
	if (dir_i_) 
	    end_slur_l_arr_[i]->dir_i_ = dir_i_;
	typeset_element(end_slur_l_arr_[i]);
    }
    end_slur_l_arr_.set_size(0);
}

Slur_register::~Slur_register()
{
    for (int i=0; i < requests_arr_.size(); i++) {
	warning("unterminated slur", requests_arr_[i]->defined_ch_c_l_m);
    }
}
