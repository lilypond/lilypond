/*
  staff-column.cc -- implement Staff_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "staff.hh"
#include "voice.hh"
#include "time-description.hh"
#include "score-column.hh"
#include "staff-column.hh"
#include "commandrequest.hh"
#include "musicalrequest.hh"
#include "interval.hh"
#include "pscore.hh"
#include "item.hh"
#include "pcol.hh"
#include "voice-element.hh"

void
Staff_column::OK() const
{
#ifndef NDEBUG
    assert (command_column_l_->when() == musical_column_l_->when());
#endif
}

Moment
Staff_column::when() const
{
    return (command_column_l_)?
	command_column_l_->when():
	musical_column_l_->when();
}

void
Staff_column::add(Voice_element*ve)
{
    for (iter_top(ve->reqs,j); j.ok(); j++) {
	if (j->command()) {
	    Command_req * c_l = j->command();
	    if (c_l->timing()) {
		timing_req_l_arr_.push(j->command()->timing());
	    }
	    if (c_l->groupchange())
		creationreq_l_arr_.push(c_l);
	    else if (!c_l->barcheck() &&  !c_l->partial() &&
		!c_l->measuregrouping())
		setup_one_request(j);	// no need to bother children
	} else {
	    if (j->rhythmic()) {
		musical_column_l_->add_duration(j->rhythmic()->duration());
	    }
	    if (!j->musical()->skip())
		setup_one_request(j);
	}
    }
}

Staff_column::Staff_column()
{
    musical_column_l_ = 0;
    command_column_l_ = 0;
    staff_l_ = 0;
}




Staff_column::~Staff_column()
{
}

void
Staff_column::set_cols(Score_column*c1, Score_column*c2)
{
    command_column_l_ = c1;
    musical_column_l_ = c2;
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
    Score_column * scorecolumn_l = musical_column_l_;
    musical_column_l_->pcol_l_->pscore_l_->typeset_item(i, scorecolumn_l->pcol_l_,
							staff_l_->pstaff_l_);
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
/*
  UGR
  This still sux
  */
void
Staff_column::typeset_breakable_items(Array<Item *> &pre_p_arr,
				      Array<Item *> &nobreak_p_arr,
				      Array<Item *> &post_p_arr)
{
    PCol * c= command_column_l_->pcol_l_;
    PScore *ps_l=command_column_l_->pcol_l_->pscore_l_;
    
    if (!c->breakable_b()) {	  
	for  (int i =0; i < pre_p_arr.size(); i++)
	    delete pre_p_arr[i];
	pre_p_arr.set_size(0);
	for  (int i =0; i < post_p_arr.size(); i++)
	    delete post_p_arr[i];
	post_p_arr.set_size(0);
    }

      
    for  (int i =0; i < pre_p_arr.size(); i++) {
	ps_l->typeset_item(pre_p_arr[i], c, staff_l_->pstaff_l_,0);
    }
    for  (int i =0; i < nobreak_p_arr.size(); i++) {
	ps_l->typeset_item(nobreak_p_arr[i], c, staff_l_->pstaff_l_,1);
    }
    for  (int i =0; i < post_p_arr.size(); i++) {
	ps_l->typeset_item(post_p_arr[i], c, staff_l_->pstaff_l_,2);
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
