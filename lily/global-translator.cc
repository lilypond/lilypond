/*
  global-translator.cc -- implement 

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "global-translator.hh"

Global_translator::Global_translator()
{
  last_mom_ = 0;
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

void
Global_translator::modify_next (Moment &w)
{
  while (extra_mom_pq_.size() && 
	 extra_mom_pq_.front() <= w)
	
    w =extra_mom_pq_.get();
}

int
Global_translator::moments_left_i() const
{
  return extra_mom_pq_.size();
}

void
Global_translator::prepare (Moment m)
{
  now_mom_ = m;
}

Moment
Global_translator::now_moment () const
{
  return now_mom_;
}

IMPLEMENT_IS_TYPE_B1(Global_translator, Translator_group);

Music_output*
Global_translator::get_output_p()
{
  return 0;
}
