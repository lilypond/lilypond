#include "request.hh"
#include "beam.hh"
#include "stem.hh"
#include "register.hh"
#include "grouping.hh"
#include "complexwalker.hh"
#include "debug.hh"
#include "complexstaff.hh"
#include "textspanner.hh"

Stem_beam_register::Stem_beam_register(Complex_walker*w)
    :Request_register(w)
{
    do_post_move_process();
    current_grouping = 0;
    beam_p_ = 0;
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
	    walk_l_->col()->tdescription_->whole_in_measure))
	    return false;

	if (stem_req_l_ && Stem_req::compare(*stem_req_l_, *req_l->stem()))
	    return false;

	stem_req_l_ = req_l->stem();
	return true;
    }
    return false;
}

void
Stem_beam_register::process_request()
{
    if (beam_req_l_) {
	if (beam_req_l_->spantype == Span_req::STOP)
	    end_beam_b_ = true;
	else {
	    beam_p_ = new Beam;
//	    walk_l_->announce_element(Staff_elem_info(beam_p_, ))
	    current_grouping = new Rhythmic_grouping;
	    if (beam_req_l_->nplet) {
		Text_spanner* t = new Text_spanner();
		t->set_support(beam_p_);
		t->spec.align_i_ = 0;
		t->spec.text_str_ = beam_req_l_->nplet;
		walk_l_->typeset_element(t);
	    }
	     
	}
    }

    if (stem_req_l_) {
	stem_p_ = new Stem(4);
	if (current_grouping)
	    current_grouping->add_child(
		walk_l_->col()->tdescription_->whole_in_measure,
		stem_req_l_->duration());

	stem_p_->flag = stem_req_l_->balltype;

	if (beam_p_) {
	    beam_p_->add(stem_p_);
	    stem_p_->print_flag = false;
	} else {
	    stem_p_->print_flag = true;
	}
	
	walk_l_->announce_element(Staff_elem_info(stem_p_,
						  stem_req_l_,  this));
    }
}

void
Stem_beam_register::acknowledge_element(Staff_elem_info info)
{
    if (!stem_p_)
	return;

    if (info.elem_p_->name() == String("Notehead") &&
	stem_req_l_->duration() == info.req_l_->rhythmic()->duration())
	
	stem_p_->add((Notehead*)info.elem_p_);
}

void
Stem_beam_register::do_pre_move_process()
{
    if (stem_p_) {
	walk_l_->typeset_element(stem_p_);
	stem_p_ = 0;
    }
    if (beam_p_ && end_beam_b_) {
	walk_l_->default_grouping.extend(current_grouping->interval());
	beam_p_->set_grouping(walk_l_->default_grouping, *current_grouping);
	walk_l_->typeset_element(beam_p_);
	delete current_grouping;
	current_grouping = 0;
	beam_p_ = 0;
    }
    end_beam_b_ = false;
}
void
Stem_beam_register::do_post_move_process()
{
    stem_p_ = 0;
    beam_req_l_ = 0;
    stem_req_l_ = 0;
    end_beam_b_ = false;
}

Stem_beam_register::~Stem_beam_register()
{
    if (beam_p_)
	error("unterminated beam");
}
