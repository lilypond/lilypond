/*
  global-acceptor.cc -- implement 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "music.hh"
#include "global-acceptor.hh"
#include "score.hh"
#include "score-column.hh"

Global_acceptor::Global_acceptor()
{
    score_l_ = 0;
    last_mom_ = 0;
}

Acceptor*
Global_acceptor::ancestor_l(int)
{
    return this;
}

void
Global_acceptor::add_moment_to_process(Moment m)
{
    if (m  > last_mom_)
	return;
    
    for (int i=0; i <  extra_mom_pq_.size(); i++)
	if (extra_mom_pq_[i] == m)
	    return;
    extra_mom_pq_.insert(m);
}

int
Global_acceptor::depth_i()const
{
    return 0;
}

void
Global_acceptor::set_score(Score *s)
{
    score_l_ = s;
    last_mom_ = score_l_->music_p_->time_int().max();
}

void
Global_acceptor::modify_next(Moment &w)
{
    while (extra_mom_pq_.size() && 
	extra_mom_pq_.front() <= w)
	
	w =extra_mom_pq_.get();
}

int
Global_acceptor::moments_left_i()const
{
    return extra_mom_pq_.size();
}

void
Global_acceptor::prepare(Moment)
{
}
