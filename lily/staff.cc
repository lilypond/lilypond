/*
  staff.cc -- implement Staff

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "input-register.hh"
#include "proto.hh"
#include "staff.hh"
#include "score.hh"
#include "voice.hh"
#include "staff-column.hh"
#include "score-column.hh"
#include "voice-element.hh"
#include "debug.hh"
#include "musical-request.hh"
#include "command-request.hh" // todo


void
Staff::add(Pointer_list<Voice*> const &l)
{
    for (iter_top(l,i); i.ok(); i++)
	voice_list_.bottom().add(i);
}

Paper_def *
Staff::paper() const
{
    return score_l_->paper_p_;
}

void
Staff::clean_cols()
{
#if 0 // TODO
    iter_top(cols_,i);
    for(; i.ok(); ){
	if (!i->musical_column_l_->used_b())
	    i->musical_column_l_ = 0;
	if (!i->command_column_l_->used_b())
	    i->command_column_l_ =0;
	
	if (!i->command_column_l_&& !i->musical_column_l_)
	    delete i.remove_p();
	else
	    i++;
    }
#endif
}


void
Staff::OK() const
{
#ifndef NDEBUG
    cols_.OK();
    voice_list_.OK();
    iter_top(cols_, i);
    iter_top(cols_, j);
    i++;
    assert(score_l_);
#endif    
}


Moment
Staff::last() const
{
    Moment l = 0;
    for (iter_top(voice_list_,i); i.ok(); i++) {
	l = l >? i->last();
    }
    return l;
}

void
Staff::print() const
{
#ifndef NPRINT
    mtor << "Staff {\n";
    for (iter_top(voice_list_,i); i.ok(); i++) {
	i->print();	
    }
    ireg_p_->print();
    mtor <<"}\n";
#endif
}

Staff::Staff()
{    
    ireg_p_ =0;
    score_l_ =0;
    pscore_l_ =0;
    pstaff_l_ =0;
}

void
Staff::add_col(Staff_column*c_l)
{
    cols_.bottom().add(c_l);
    c_l->staff_l_ = this;
}
