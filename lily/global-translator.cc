/*
  global-translator.cc -- implement 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "music.hh"
#include "global-translator.hh"
#include "score.hh"
#include "score-column.hh"

Global_translator::Global_translator()
{
    score_l_ = 0;
    last_mom_ = 0;
}

Translator*
Global_translator::ancestor_l (int)
{
    return this;
}

void
Global_translator::add_moment_to_process (Moment m)
{
    if (m  > last_mom_)
	return;
    
    for (int i=0; i <  extra_mom_pq_.size(); i++)
	if (extra_mom_pq_[i] == m)
	    return;
    extra_mom_pq_.insert (m);
}

int
Global_translator::depth_i()const
{
    return 0;
}

void
Global_translator::set_score (Score *s)
{
    score_l_ = s;
    last_mom_ = score_l_->music_p_->time_int().max ();
}

void
Global_translator::modify_next (Moment &w)
{
    while (extra_mom_pq_.size() && 
	extra_mom_pq_.front() <= w)
	
	w =extra_mom_pq_.get();
}

int
Global_translator::moments_left_i()const
{
    return extra_mom_pq_.size();
}

void
Global_translator::prepare (Moment)
{
}


IMPLEMENT_IS_TYPE_B1(Global_translator, Translator);
