/*
  stem-beam-reg.cc -- part of GNU LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musical-request.hh"
#include "stem-beam-reg.hh"
#include "beam.hh"
#include "stem.hh"
#include "grouping.hh"
#include "text-spanner.hh"
#include "debug.hh"
#include "grouping.hh"
#include "note-head.hh"
#include "time-description.hh"

Stem_beam_register::Stem_beam_register()
{
    do_post_move_processing();

    current_grouping = 0;
    beam_p_ = 0;
    default_dir_i_ =0;
    start_req_l_ = 0;
}

bool
Stem_beam_register::do_try_request(Request*req_l)
{
    
    Musical_req* mus_l = req_l->musical();
    /* Debiele puntkomma's. Laat je er eentje per ongeluk achter een
      if(..) staan, lijkt het net op een luis in gcc.

      (ofwel Python rules)
      */
    if (!mus_l)
	return false;


    if ( mus_l->beam() ) {
	if (bool(beam_p_ ) == bool(mus_l->beam()->spantype == Span_req::START))
	    return false;
	
	if (beam_req_l_ && Beam_req::compare(*beam_req_l_ , *mus_l->beam()))
	    return false;
	
	beam_req_l_ = mus_l->beam();
	return true;
    }
    
    if ( mus_l->stem() ) {
	if (current_grouping && !current_grouping->child_fit_b(
	    get_staff_info().time_C_->whole_in_measure_))
	    return false;

	if (stem_req_l_ && Stem_req::compare(*stem_req_l_, *mus_l->stem()))
	    return false;

	stem_req_l_ = mus_l->stem();
	return true;
    }
    return false;
}

void
Stem_beam_register::do_process_requests()
{
    if (beam_req_l_) {
	if (beam_req_l_->spantype == Span_req::STOP) {
	    end_beam_b_ = true;
	    start_req_l_ = 0;
	} else {
	    beam_p_ = new Beam;
	    start_req_l_ = beam_req_l_;
	    beam_p_->left_col_l_ = get_staff_info().musical_pcol_l();
	    current_grouping = new Rhythmic_grouping;
	    if (beam_req_l_->nplet) {
		Text_spanner* t = new Text_spanner();
		Text_def *defp = new Text_def;
		t->set_support(beam_p_);
		defp->align_i_ = 0;
		defp->text_str_ = beam_req_l_->nplet;
		defp->style_str_="italic";
		t->spec_p_  = defp;
		typeset_element(t);
	    }
	     
	}
    }

    if (stem_req_l_) {
	stem_p_ = new Stem(8);
	if (current_grouping)
	    current_grouping->add_child(
		get_staff_info().time_C_->whole_in_measure_,
		stem_req_l_->duration());

	stem_p_->flag_i_ = stem_req_l_->duration_.type_i_;

	if (beam_p_) {
	    if (stem_req_l_->duration_.type_i_<= 4)
		stem_req_l_->warning( "stem doesn't fit in Beam");
	    else
		beam_p_->add(stem_p_);
	    stem_p_->print_flag_b_ = false;
	} else {
	    stem_p_->print_flag_b_ = true;
	}
	
	announce_element(Score_elem_info(stem_p_, stem_req_l_));
    }
}

void
Stem_beam_register::acknowledge_element(Score_elem_info info)
{
    if (!stem_p_)
	return;

    if (info.elem_l_->name() == Note_head::static_name() &&
	stem_req_l_->duration() 
	== info.req_l_->musical()->rhythmic()->duration()){
	Note_head * n_l= (Note_head*)info.elem_l_->item();
	stem_p_->add(n_l);
    }
}
void
Stem_beam_register::do_pre_move_processing()
{
    if (stem_p_) {
	if (default_dir_i_)
	    stem_p_->dir_i_ = default_dir_i_;
	
	typeset_element(stem_p_);
	stem_p_ = 0;
    }
    if (beam_p_ && end_beam_b_) {
	Rhythmic_grouping const * rg_C = get_staff_info().rhythmic_C_;
	rg_C->extend(current_grouping->interval());
	beam_p_->set_grouping(*rg_C, *current_grouping);
	beam_p_->right_col_l_ = get_staff_info().musical_pcol_l();
	typeset_element(beam_p_);
	delete current_grouping;
	current_grouping = 0;
	beam_p_ = 0;
    }
    end_beam_b_ = false;
}
void
Stem_beam_register::do_post_move_processing()
{
    stem_p_ = 0;
    beam_req_l_ = 0;
    stem_req_l_ = 0;
    end_beam_b_ = false;
}

Stem_beam_register::~Stem_beam_register()
{
    if (beam_p_)
	start_req_l_->warning("unterminated beam");
}

void
Stem_beam_register::set_feature(Feature i)
{
    if (i.type_ == "vdir")	
	default_dir_i_ = i.value_;
}

IMPLEMENT_STATIC_NAME(Stem_beam_register);
IMPLEMENT_IS_TYPE_B1(Stem_beam_register,Request_register);
ADD_THIS_REGISTER(Stem_beam_register);
