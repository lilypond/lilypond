/*
  score-walker.cc -- implement Score_walker

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "proto.hh"
#include "plist.hh"
#include "debug.hh"
#include "score-walker.hh"
#include "score.hh"
#include "staff-walker.hh"
#include "staff.hh"
#include "score-column.hh"

Score_walker::Score_walker(Score *s)
    :PCursor<Score_column *> (s->cols_)
{
    score_l_ = s;
    for (iter_top(s->staffs_,i); i.ok(); i++) {
	Staff_walker* w_p=i->get_walker_p();
	w_p->score_walk_l_ =this;
	walker_p_arr_.push(w_p);
    }

    reinit();
    breaks_i_=0;
}


void
Score_walker::reinit()
{
    disallow_break_walk_l_arr = walker_p_arr_;
    disallow_break_count_ = disallow_break_walk_l_arr.size();
}


/** Advance the cursor, and all Staff_walkers contained in this. Reset
  runtime fields */
void 
Score_walker::operator ++(int )
{
    Moment last = ptr()->when();
    
    PCursor<Score_column *>::operator++(0);
    if (ok() && ptr()->when() == last)
	PCursor<Score_column *>::operator++(0);
    reinit();
    bool last_b =  (!ok());	// ughh
    for (int i=0; i< walker_p_arr_.size(); i++) {
	if (walker_p_arr_[i]->ok() &&
	    (last_b || walker_p_arr_[i]->when() < when())) {

	    walker_p_arr_[i]->operator++(0);
	}
    }
}

/** Allow the command_column to be breakable for one staff. If all
  staffs allow, then allow a break here.  */
void
Score_walker::allow_break(Staff_walker*w)
{
    for (int i=0; i < disallow_break_walk_l_arr.size(); i++) {
	if (w == disallow_break_walk_l_arr[i]) {
	    disallow_break_count_ --;
	    disallow_break_walk_l_arr[i] =0;

	    if (!disallow_break_count_) {
		PCursor<Score_column*> col_cursor = *this;
		if (ptr()->musical_b())
		    col_cursor --;
		col_cursor->set_breakable();
	    }
	}
    }
}

bool
Score_walker::break_allowed_b()
{
    return !disallow_break_count_;
}

Moment
Score_walker::when()
{
    return ptr()->when();
}

void
Score_walker::process()
{
    for (int i=0; i < walker_p_arr_.size(); i++) {
	Staff_walker *w = walker_p_arr_[i];
	if ( w->ok() && w->when() == when() ) {
	    walker_p_arr_[i]->process();
	}
    }
    if (break_allowed_b()){
	breaks_i_ ++;
	if (! (breaks_i_ % 8)) 
	    *mlog << "[" <<breaks_i_<<"]"<<flush;
    }
}

Score_walker::~Score_walker()
{
    *mlog << "[" <<breaks_i_<<"]"<<flush;
    for (int i=0; i < walker_p_arr_.size(); i++) 
	delete walker_p_arr_[i];
}


