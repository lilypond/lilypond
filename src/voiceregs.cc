#include "rest.hh"
#include "notehead.hh"
#include "paper.hh"
#include "debug.hh"
#include "slur.hh"
#include "request.hh"
#include "complexwalker.hh"
#include "complexstaff.hh"
#include "voicegroup.hh"
#include "register.hh"

Voice_registers::Voice_registers(Complex_walker*c_l, Voice *v_p)
    : head_reg_(c_l), slur_reg_(c_l)
{
    voice_l_ = v_p;
}

bool
Voice_registers::try_request(Request * r_l)
{
    bool b = head_reg_.try_request(r_l);
    if (!b)
	b = slur_reg_.try_request(r_l);
    return b;
}

void
Voice_registers::announce_element(Staff_elem_info i)
{
    if (i.voice_l_ != voice_l_)
	return;
    if (i.origin_reg_l_ != &slur_reg_)
	slur_reg_.acknowledge_element(i);
}

void
Voice_registers::pre_move_processing()
{
    head_reg_.pre_move_processing();
    slur_reg_.pre_move_processing();
}
void
Voice_registers::post_move_processing()
{
    head_reg_.post_move_processing();
    slur_reg_.post_move_processing();
}

void
Voice_registers::process_requests()
{
    head_reg_.process_request();
    slur_reg_.process_request();
}

bool
Voice_registers::acceptable_request(Request*r)
{
    return (r->rest() || r->note() || r->slur());
    
}
/****************/

Notehead_register::Notehead_register(Complex_walker*w_l)
    :Request_register(w_l)
{
    note_p_ = 0;
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
Notehead_register::process_request()
{
    if (!accepted_req_arr_.size())
	return;
    
    Request* req_l = accepted_req_arr_.top();
    if (req_l->note()) {
	Notehead*n_p = new Notehead(8);	// ugh
	note_p_ = n_p;
	n_p->set_rhythmic(req_l->rhythmic());
	n_p->position = req_l->note()->height() + -2;
    } else {
	note_p_ = new Rest ( req_l->rhythmic()->balltype,
			     req_l->rhythmic()->dots);
	if (req_l->rhythmic()->balltype <= 2)
	    note_p_->translate(
		Offset(0,
		       6 * walk_l_->staff()->paper()->internote()));
    }
    Staff_elem_info itinf(note_p_,req_l,this);
    walk_l_->announce_element(itinf);
}

void
Notehead_register::do_pre_move_process()
{
    if (note_p_) {
	walk_l_->typeset_element(note_p_);
	note_p_ = 0;
    }
}

/****************/

Slur_register::Slur_register(Complex_walker* w)
    : Request_register(w)
{
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
	    walk_l_->announce_element(Staff_elem_info(s_p, slur_req_l, this));
	}
    }
    for (int i=0; i < start_slur_l_arr_.size(); i++)
	slur_l_stack_.push(start_slur_l_arr_[i]);
}

void
Slur_register::do_pre_move_process()
{
    for (int i = 0; i < end_slur_l_arr_.size(); i++)
	walk_l_->typeset_element(end_slur_l_arr_[i]);
    end_slur_l_arr_.set_size(0);
}

Slur_register::~Slur_register()
{
    for (int i=0; i < requests_arr_.size(); i++) {
	warning("unterminated slur", requests_arr_[i]->defined_ch_c_l_m);
    }
}
