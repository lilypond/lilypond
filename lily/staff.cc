/*
  staff.cc -- implement Staff

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "input-register.hh"
#include "staff.hh"
#include "score.hh"
#include "voice.hh"
#include "staff-column.hh"
#include "score-column.hh"
#include "voice-element.hh"
#include "debug.hh"
#include "musical-request.hh"
#include "command-request.hh" // todo
#include "staffline.hh"
#include "complex-walker.hh"
#include "super-elem.hh"
#include "p-score.hh"
#include "scoreline.hh"

void
Staff::add(Link_list<Voice*> const &l)
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

Staff::~Staff()
{
    delete ireg_p_;
}

Staff::Staff()
{    
    ireg_p_ =0;
    score_l_ =0;
    pscore_l_ =0;
}

void
Staff::add_col(Staff_column*c_l)
{
    cols_.bottom().add(c_l);
    c_l->staff_l_ = this;
}

void
Staff::set_output(PScore* pscore_l )
{
    pscore_l_ = pscore_l;
    staff_line_l_ = new Line_of_staff;
    pscore_l_->typeset_unbroken_spanner(staff_line_l_);
    pscore_l_->super_elem_l_->line_of_score_l_->add_line(staff_line_l_);
}


Staff_walker * 
Staff::get_walker_p()
{
    return new Complex_walker(this);
}
