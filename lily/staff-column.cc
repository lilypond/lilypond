/*
  staff-column.cc -- implement Staff_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "proto.hh"
#include "plist.hh"
#include "staff.hh"
#include "voice.hh"
#include "time-description.hh"
#include "score-column.hh"
#include "staff-column.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "interval.hh"
#include "p-score.hh"
#include "item.hh"
#include "p-col.hh"
#include "request-column.hh"
#include "grouping.hh"

void
Staff_column::OK() const
{
#ifndef NDEBUG
    
#endif
}

Moment
Staff_column::when() const
{
    return req_col_l_->when();
}

void
Staff_column::add_reqs(Array<Request*> req_l_arr)
{
    for (int i=0; i < req_l_arr.size(); i++) {
	Request * j = req_l_arr[i];
	if (j->command()) {
	    Command_req * c_l = j->command();
	    if (c_l->timing()) {
		timing_req_l_arr_.push(j->command()->timing());
	    }
	    if (c_l->groupchange())
		creationreq_l_arr_.push(c_l);
	    else if (!c_l->barcheck() &&  !c_l->partial() &&
		!c_l->measuregrouping())
		setup_one_request(j);	
	} else {
	    if (j->rhythmic()) {
		req_col_l_->musical_column_l_->add_duration(j->rhythmic()->duration());
	    }
	    if (j->musical()) {
		Musical_req*m = j->musical();
		if(m->skip())
		    continue;
	    }
	    setup_one_request(j);
	}
    }
}

Staff_column::Staff_column()
{
    staff_l_ = 0;
}




Staff_column::~Staff_column()
{
}

void
Staff_column::set_req_col(Request_column *col_l)
{
    req_col_l_ = col_l;
}

void
Staff_column::setup_one_request(Request * j)
{
    if (j->command()) // ugh
	commandreq_l_arr_.push(j);
    else if (j->musical())
	musicalreq_l_arr_.push(j);
}

void
Staff_column::typeset_musical_item(Item*i)
{
    assert(i);
    Score_column * scorecolumn_l = req_col_l_->musical_column_l_;
    scorecolumn_l->pcol_l_->pscore_l_->typeset_item(i, scorecolumn_l->pcol_l_);
}

/**
  align items in #item_l_arr#,

  @return the width of the items after aligning.
 */
Interval
align_items(Array<Item*> item_l_arr)
{
    Interval wid(0,0);
    for  (int i =0; i < item_l_arr.size(); i++) {
	Interval item_width= item_l_arr[i]->width();
	if (item_width.empty_b()) {
	    item_width = Interval(0,0);
	}
	Real dx =wid.right - item_width.left;
	item_width += dx;
	item_l_arr[i]->translate(Offset(dx ,0));
	wid.unite(item_width);
    }
    return wid;
}

void 
translate_items(Real x,  Array<Item*> item_l_arr)
{
    for  (int i =0; i < item_l_arr.size(); i++) 
	item_l_arr[i]->translate(Offset(x, 0));
}
/**
  TODO: 
  Write a "horizontal align" item, which aligns the pres, nobreaks, posts, etc.
  
  */
void
Staff_column::typeset_breakable_items(Array<Item *> &pre_p_arr,
				      Array<Item *> &nobreak_p_arr,
				      Array<Item *> &post_p_arr)
{
    Score_column * scol_l= req_col_l_->command_column_l_;
    PCol * c= scol_l->pcol_l_;
    PScore *ps_l=scol_l->pcol_l_->pscore_l_;
    
    if (!c->breakable_b()) {	  
	for  (int i =0; i < pre_p_arr.size(); i++) {
	    pre_p_arr[i]->unlink();
	    delete pre_p_arr[i];
	}
	pre_p_arr.set_size(0);
	for  (int i =0; i < post_p_arr.size(); i++) {
	    post_p_arr[i]->unlink();
	    delete post_p_arr[i];
	}
	post_p_arr.set_size(0);
    }

      
    for  (int i =0; i < pre_p_arr.size(); i++) {
	ps_l->typeset_item(pre_p_arr[i], c,0);
    }
    for  (int i =0; i < nobreak_p_arr.size(); i++) {
	ps_l->typeset_item(nobreak_p_arr[i], c, 1);
    }
    for  (int i =0; i < post_p_arr.size(); i++) {
	ps_l->typeset_item(post_p_arr[i], c, 2);
    }

    Interval pre_wid= align_items(pre_p_arr);
    translate_items( -pre_wid.right, pre_p_arr);
    align_items(nobreak_p_arr);
    Interval post_wid =align_items(post_p_arr);
    translate_items (-post_wid.left , post_p_arr);

    pre_p_arr.set_size(0);
    post_p_arr.set_size(0);
    nobreak_p_arr.set_size(0);
}

Score_column*
Staff_column::command_column_l()
{
    return req_col_l_->command_column_l_;
}

Score_column*
Staff_column::musical_column_l()
{
    return req_col_l_->musical_column_l_;
}

void
Staff_column::update_time(Time_description &time_, 
		    Rhythmic_grouping *default_grouping)
{
    // first all meter changes
    for (int i=0; i < timing_req_l_arr_.size(); i++) {
	Timing_req * tr_l = timing_req_l_arr_[i];
	if (tr_l->meterchange()) {
	    int b_i=tr_l->meterchange()->beats_i_;
	    int o_i = tr_l->meterchange()->one_beat_i_;
	    if (! time_.allow_meter_change_b() )
		tr_l->warning("Meter change not allowed here");
	    else{
		time_.set_meter(b_i, o_i);
		if (default_grouping)
		    *default_grouping = 
		    Rhythmic_grouping(MInterval(0,Moment(b_i, o_i)), b_i);
	    }
	}
    }
    
    // then do the rest
    for (int i=0; i < timing_req_l_arr_.size(); i++) {
	Timing_req * tr_l = timing_req_l_arr_[i];
	if (tr_l->partial()) {
	    Moment m = tr_l->partial()->duration_;
	    String error = time_.try_set_partial_str(m);
	    if (error != "") {
		tr_l->warning(error);
	    } else 
		time_.setpartial(m);
	} else if (tr_l->barcheck() && time_.whole_in_measure_) {
	    tr_l ->warning( "Barcheck failed");

	    time_.whole_in_measure_ = 0; // resync
	    time_.error_b_ = true;
	} else if (tr_l->cadenza()) {
	    time_.set_cadenza(tr_l->cadenza()->on_b_);
	} else if (tr_l->measuregrouping()) {
	    if (default_grouping)
		*default_grouping = parse_grouping(
		    tr_l->measuregrouping()->beat_i_arr_,
		    tr_l->measuregrouping()->elt_length_arr_);
	}
    }
    time_.OK();
}   

