/*
  stcol.cc -- implement Staff_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "voice.hh"
#include "timedescription.hh"
#include "sccol.hh"
#include "stcol.hh"
#include "commandrequest.hh"
#include "musicalrequest.hh"

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
	if (j->nonmus()) {
	    if (j->nonmus()->timing()) {
		timing_req_l_arr_.push(j->nonmus()->timing());
	    }
	    if (!j->barcheck() && !j->nonmus()->measuregrouping())
		setup_one_request(j);	// no need to bother children
	} else {
	    if (j->rhythmic()) {
		musical_column_l_->add_duration(j->rhythmic()->duration());
	    }
	    setup_one_request(j);
	}
    }
}

Staff_column::Staff_column()
{
    musical_column_l_ = 0;
    command_column_l_ = 0;
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
