/*
  input-score.cc -- implement Input_score

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "input-score.hh"
#include "input-staff.hh"
#include "input-music.hh"
#include "score.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "staff.hh"


void
Input_score::add(Input_staff*s)
{
    staffs_.bottom().add(s);
}

void
Input_score::set(Paper_def*p)
{
    delete paper_p_;
    paper_p_ = p;
}

void
Input_score::set(Midi_def* midi_p)
{
    delete midi_p_;
    midi_p_ = midi_p;
}
 
Input_score::Input_score(Input_score const&s)
    : Input(s)
{
    paper_p_ = (s.paper_p_)? new Paper_def(*s.paper_p_) :0;
    midi_p_ = (s.midi_p_)? new Midi_def(*s.midi_p_) : 0;
    errorlevel_i_ = s.errorlevel_i_;
}

Score*
Input_score::parse()
{
    Score *s_p = new Score;

    s_p->errorlevel_i_ = errorlevel_i_;
    if (midi_p_)
	s_p->set(new Midi_def(*midi_p_));
    if (paper_p_)
	s_p->set(new Paper_def(*paper_p_));

    for (iter_top(staffs_,i); i.ok(); i++) {
	Staff* staf_p=i->parse(s_p);
	s_p->add(staf_p);
    }

    return s_p;
}


Input_score::~Input_score()
{
    delete paper_p_;
    delete midi_p_;
}

Input_score::Input_score()
{
    paper_p_= 0;
    midi_p_ = 0;
    errorlevel_i_ = 0;
}

void
Input_score::print()const
{
#ifndef NPRINT
    mtor << "Input_score {\n";
    for (iter_top(staffs_,i); i.ok(); i++) {
	i->print();
    }
    mtor << "}\n";
#endif
}
