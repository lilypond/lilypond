/*
  inputscore.cc -- implement Input_score

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "inputscore.hh"
#include "inputstaff.hh"
#include "inputmusic.hh"
#include "score.hh"
#include "paperdef.hh"
#include "staff.hh"


void
Input_score::add(Input_staff*s)
{
    staffs_.bottom().add(s);
}

void
Input_score::set(Paperdef*p)
{
    delete paper_p_;
    paper_p_ = p;
}

Input_score::Input_score(Input_score const&s)
{
    paper_p_ = (s.paper_p_)? new Paperdef(*s.paper_p_) :0;
    defined_ch_c_l_ = s.defined_ch_c_l_;
    errorlevel_i_ = s.errorlevel_i_;
    score_wide_music_p_ = (s.score_wide_music_p_) ?
	s.score_wide_music_p_->clone():0;
}

Score*
Input_score::parse()
{
    Paperdef* paper_p=new Paperdef(*paper_p_);
    Score *s_p = new Score(paper_p);
    s_p->defined_ch_c_l_= defined_ch_c_l_;
    s_p->errorlevel_i_ = errorlevel_i_;

    for (iter_top(staffs_,i); i.ok(); i++) {
	Staff* staf_p=i->parse(s_p, score_wide_music_p_);
	s_p->add(staf_p);
    }

    return s_p;
}

void
Input_score::set(Input_music *m_p)
{
    delete score_wide_music_p_;
    score_wide_music_p_ =m_p;    
}


Input_score::~Input_score()
{
    delete paper_p_;
    delete score_wide_music_p_;
}

Input_score::Input_score()
{
    score_wide_music_p_ =0;
    defined_ch_c_l_=0;
    paper_p_= 0;
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
