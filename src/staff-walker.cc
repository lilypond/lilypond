/*
  staff-walker.cc -- implement Staff_walker

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "grouping.hh"
#include "staff.hh"
#include "musicalrequest.hh"
#include "staff-walker.hh"
#include "staff-column.hh"
#include "score-column.hh"
#include "debug.hh"
#include "time-description.hh"
#include "commandrequest.hh"
#include "grouping.hh"
#include "score-walker.hh"

Staff_walker::~Staff_walker()
{
    do_pre_move();
}

Staff_walker::Staff_walker(Staff_walker const &s)
    :PCursor<Staff_column*> (s)
{
    assert(false);
}

Staff_walker::Staff_walker(Staff * s, PScore*ps )
    : PCursor<Staff_column*> (s->cols_)
{
    staff_l_ = s;
    pscore_l_ = ps;
    
    // should be in tdes. TODO
    default_grouping = new Rhythmic_grouping(MInterval(0, 1), 4); 
    score_walk_l_ = 0;
}

Moment
Staff_walker::when() const
{
    return ptr()->when();
}


void
Staff_walker::process_timing_reqs()
{
    for (int i=0; i < ptr()->timing_req_l_arr_.size(); i++) {
	Timing_req * tr_l = ptr()->timing_req_l_arr_[i];
	if (tr_l->meterchange()) {
	    int b_i=tr_l->meterchange()->beats_i_;
	    int o_i = tr_l->meterchange()->one_beat_i_;
	    time_.set_meter(b_i, o_i);
			
	    *default_grouping = Rhythmic_grouping(
		MInterval(0,Moment(b_i, o_i)), b_i);
	} 
    }
    
    for (int i=0; i < ptr()->timing_req_l_arr_.size(); i++) {
	Timing_req * tr_l = ptr()->timing_req_l_arr_[i];
	if (tr_l->partial()) {
	    time_.setpartial(tr_l->partial()->duration_);
	} else if (tr_l->barcheck() && time_.whole_in_measure_) {
	    warning( "Barcheck failed", tr_l->defined_ch_c_l_ );
	} else if (tr_l->cadenza()) {
	    time_.set_cadenza(tr_l->cadenza()->on_b_);
	} else if (tr_l->measuregrouping()) {
	    *default_grouping = parse_grouping(
		tr_l->measuregrouping()->beat_i_arr_,
		tr_l->measuregrouping()->elt_length_arr_);
	}
    }
    time_.OK();
}

void
Staff_walker::operator++(int i)
{
    Moment last = when();

    do_pre_move();
    PCursor<Staff_column*>::operator++(i);
    if (ok() ) {
	Moment delta_t = when() - last;
	assert(delta_t >0);
	time_.add( delta_t );
    }
    do_post_move();
}

void
Staff_walker::process()
{
    process_timing_reqs();    
    process_requests();
}

void 
Staff_walker::allow_break()
{
    score_walk_l_->allow_break(this);
}
 
