/*
  dynamic-reg.cc -- implement Dynamic_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "debug.hh"
#include "crescendo.hh"
#include "dynamic-reg.hh"
#include "musical-request.hh"
#include "text-item.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "score-column.hh"
#include "staff-sym.hh"

Dynamic_register::Dynamic_register()
{
    dir_i_ =0;
    post_move_processing();
    dynamic_p_ =0;
    to_end_cresc_p_ = cresc_p_ = 0;
    cresc_req_l_ = 0;
}

void
Dynamic_register::post_move_processing()
{
    dynamic_req_l_arr_.set_size(0);
}

bool    
Dynamic_register::try_request(Request * r)
{
    Musical_req * m = r->musical();
    if (!m || !m->dynamic())
	return false;
    dynamic_req_l_arr_.push(m->dynamic());
    return true;
}
void
Dynamic_register::process_requests()
{
    Crescendo*  new_cresc_p=0; 
    for (int i=0; i < dynamic_req_l_arr_.size(); i++) {
	Dynamic_req *dreq_l = dynamic_req_l_arr_[i];
	if (dreq_l->absdynamic()) {
	    Text_def * td_p = new Text_def;
	    td_p->align_i_ = 0;
	    String loud =Dynamic_req::loudness_str(
		dreq_l->absdynamic()->loudness_);
	    
	    td_p->text_str_ = paper()->lookup_l()->dynamic(loud).tex;
	    td_p->style_str_ = "dynamic";

	    assert (!dynamic_p_) ; // TODO
		
	    dynamic_p_ = new Text_item(td_p);
	    announce_element(Staff_elem_info(dynamic_p_, dreq_l));
	} else if (dreq_l->span_dynamic()) {

	    Span_dynamic_req* span_l = dreq_l->span_dynamic();
	    if (span_l->spantype == Span_req::STOP) {
		if (!cresc_p_) {
		    span_l->warning("Can't find cresc to end " );
		} else {
		    assert(!to_end_cresc_p_);
		    to_end_cresc_p_ =cresc_p_;
		    cresc_p_ = 0;
		}
	    } else if (span_l->spantype == Span_req::START) {
		cresc_req_l_ = span_l;
		assert(!new_cresc_p);
		new_cresc_p  = new Crescendo;
		new_cresc_p->grow_dir_i_ = span_l->dynamic_dir_i_;
		announce_element(Staff_elem_info(new_cresc_p, span_l));
	    }
	}
    }

    if ( new_cresc_p ) {
	cresc_p_ = new_cresc_p;
	cresc_p_->left_col_l_ = get_staff_info().musical_l()->pcol_l_;
	if (dynamic_p_) {
	    cresc_p_->left_dyn_b_ = true;
	}
    }
}

void
Dynamic_register::pre_move_processing()
{
    Staff_symbol* s_l = get_staff_info().staff_sym_l_;
    if (dynamic_p_) {
	dynamic_p_->set_staffsym(s_l);
	typeset_element(dynamic_p_);
	dynamic_p_ = 0;
    }
    if ( to_end_cresc_p_) {
	if (dynamic_p_)
	    to_end_cresc_p_->right_dyn_b_=true;
	
	to_end_cresc_p_->right_col_l_ = get_staff_info().musical_l()->pcol_l_;
	to_end_cresc_p_->set_staffsym(s_l);
	typeset_element(to_end_cresc_p_);
	to_end_cresc_p_ = 0;
    }
}

bool
Dynamic_register::acceptable_request_b(Request*r)const
{
    Musical_req * m = r->musical();
    return  (m && m->dynamic());
}

void
Dynamic_register::set_feature(Features i)
{
    dir_i_ = i.direction_i_;
}

IMPLEMENT_STATIC_NAME(Dynamic_register);
ADD_THIS_REGISTER(Dynamic_register);

Dynamic_register::~Dynamic_register()
{
    delete dynamic_p_;
    delete to_end_cresc_p_;
    if (cresc_p_) {
	cresc_req_l_->warning("unended crescendo");
    }
    delete cresc_p_;
}
