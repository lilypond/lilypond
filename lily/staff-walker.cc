/*
  staff-walker.cc -- implement Staff_walker

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "grouping.hh"
#include "staff.hh"
#include "musical-request.hh"
#include "staff-walker.hh"
#include "staff-column.hh"
#include "score-column.hh"
#include "debug.hh"
#include "time-description.hh"
#include "command-request.hh"
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
    ptr()->update_time(time_, default_grouping);
}

void
Staff_walker::operator++(int i)
{
    Moment last = when();

    do_pre_move();
    PCursor<Staff_column*>::operator++(i);
    if (ok() ) {
	Moment delta_t = when() - last;
	assert(delta_t >Moment(0));
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
 
