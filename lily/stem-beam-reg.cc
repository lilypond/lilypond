/*
  stem-beam-reg.cc -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musical-request.hh"
#include "stem-beam-reg.hh"
#include "beam.hh"
#include "stem.hh"
#include "grouping.hh"
#include "text-spanner.hh"
#include "complex-walker.hh"
#include "complex-staff.hh"
#include "debug.hh"
#include "grouping.hh"
#include "notehead.hh"

Stem_beam_register::Stem_beam_register()
{
    post_move_processing();
    current_grouping = 0;
    beam_p_ = 0;
    set_feature(Features::dir(0));
    start_req_l_ = 0;
}

bool
Stem_beam_register::try_request(Request*req_l)
{
    if ( req_l->beam() ) {
	if (bool(beam_p_ ) == bool(req_l->beam()->spantype == Span_req::START))
	    return false;
	
	if (beam_req_l_ && Beam_req::compare(*beam_req_l_ , *req_l->beam()))
	    return false;
	
	beam_req_l_ = req_l->beam();
	return true;
    }
    
    if ( req_l->stem() ) {
	if (current_grouping && !current_grouping->child_fit_query(
	    get_staff_info().time_c_l_->whole_in_measure_))
	    return false;

	if (stem_req_l_ && Stem_req::compare(*stem_req_l_, *req_l->stem()))
	    return false;

	stem_req_l_ = req_l->stem();
	return true;
    }
    return false;
}

void
Stem_beam_register::process_requests()
{
    if (beam_req_l_) {
	if (beam_req_l_->spantype == Span_req::STOP) {
	    end_beam_b_ = true;
	    start_req_l_ = 0;
	} else {
	    beam_p_ = new Beam;
	    start_req_l_ = beam_req_l_;

	    current_grouping = new Rhythmic_grouping;
	    if (beam_req_l_->nplet) {
		Text_spanner* t = new Text_spanner();
		t->set_support(beam_p_);
		t->spec.align_i_ = 0;
		t->spec.text_str_ = beam_req_l_->nplet;
		t->spec.style_str_="italic";
		typeset_element(t);
	    }
	     
	}
    }

    if (stem_req_l_) {
	stem_p_ = new Stem(4);
	if (current_grouping)
	    current_grouping->add_child(
		get_staff_info().time_c_l_->whole_in_measure_,
		stem_req_l_->duration());

	stem_p_->flag = stem_req_l_->balltype;

	if (beam_p_) {
	    if (stem_req_l_->balltype<= 4)
		warning( "stem doesn't fit in Beam",
			 stem_req_l_->defined_ch_c_l_);
	    else
		beam_p_->add(stem_p_);
	    stem_p_->print_flag = false;
	} else {
	    stem_p_->print_flag = true;
	}
	
	announce_element(Staff_elem_info(stem_p_, stem_req_l_));
    }
}

void
Stem_beam_register::acknowledge_element(Staff_elem_info info)
{
    if (!stem_p_)
	return;

    if (info.elem_p_->name() == Notehead::static_name() &&
	stem_req_l_->duration() == info.req_l_->rhythmic()->duration())
	
	stem_p_->add((Notehead*)info.elem_p_);
}

void
Stem_beam_register::pre_move_processing()
{
    if (stem_p_) {
	if (default_dir_i_)
	    stem_p_->dir = default_dir_i_;
	
	typeset_element(stem_p_);
	stem_p_ = 0;
    }
    if (beam_p_ && end_beam_b_) {
	const Rhythmic_grouping * rg_c_l = get_staff_info().rhythmic_c_l_;
	rg_c_l->extend(current_grouping->interval());
	beam_p_->set_grouping(*rg_c_l, *current_grouping);
	typeset_element(beam_p_);
	delete current_grouping;
	current_grouping = 0;
	beam_p_ = 0;
    }
    end_beam_b_ = false;
}
void
Stem_beam_register::post_move_processing()
{
    stem_p_ = 0;
    beam_req_l_ = 0;
    stem_req_l_ = 0;
    end_beam_b_ = false;
}

Stem_beam_register::~Stem_beam_register()
{
    if (beam_p_)
	warning("unterminated beam", start_req_l_->defined_ch_c_l_);
}

void
Stem_beam_register::set_feature(Features i)
{
    default_dir_i_ = i.direction_i_;
}
