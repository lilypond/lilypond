/*
  global-translator.cc -- implement Global_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "global-translator.hh"
#include "music-iterator.hh"
#include "debug.hh"

Global_translator::Global_translator()
{
}

void
Global_translator::add_moment_to_process (Moment m)
{
  if (m  > final_mom_)
    return;

  if (m < now_mom_ )
    programming_error ("Trying to freeze in time.");
  
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
  prev_mom_  = now_mom_;
  now_mom_ = m;
}

Moment
Global_translator::now_mom () const
{
  return now_mom_;
}



Music_output*
Global_translator::get_output_p()
{
  return 0;
}

void
Global_translator::process ()
{
}
void
Global_translator::start ()
{
}
void
Global_translator::finish ()
{
}

void
Global_translator::run_iterator_on_me (Music_iterator * iter)
{
  while (iter->ok() || moments_left_i ())
    {
      Moment w;
      w.set_infinite (1);
      if (iter->ok())
	{
	  w = iter->next_moment();
	  DEBUG_OUT << "proccing: " << w << '\n';
	  if (flower_dstream && !flower_dstream->silent_b ("walking"))
	    iter->print();
	}
      
      modify_next (w);
      prepare (w);
      
      if (flower_dstream && !flower_dstream->silent_b ("walking"))
	print();

      iter->process_and_next (w);
      process();
    }
}
