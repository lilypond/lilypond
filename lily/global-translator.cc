/*
  global-translator.cc -- implement Global_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <stdio.h>

#include "debug.hh"
#include "music.hh"
#include "request.hh"
#include "music-list.hh"
#include "music-iterator.hh"
#include "global-translator.hh"

Global_translator::Global_translator ()
{
}

void
Global_translator::add_moment_to_process (Moment m)
{
  if (m  > final_mom_)
    return;

  if (m < now_mom_)
    programming_error ("Trying to freeze in time.");
  
  for (int i=0; i <  extra_mom_pq_.size (); i++)
    if (extra_mom_pq_[i] == m)
      return;
  extra_mom_pq_.insert (m);
}

Moment
Global_translator::sneaky_insert_extra_moment (Moment w)
{
  while (extra_mom_pq_.size () && extra_mom_pq_.front () <= w)
    w = extra_mom_pq_.get ();
  return w;
}

int
Global_translator::moments_left_i () const
{
  return extra_mom_pq_.size ();
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
Global_translator::get_output_p ()
{
  return 0;
}

void
Global_translator::one_time_step ()
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
  if (iter-> ok ())
    prev_mom_ = now_mom_ = iter->pending_moment ();

  bool first = true;
  while (iter->ok () || moments_left_i ())
    {
      Moment w;
      w.set_infinite (1);
      if (iter->ok ())
	{
	  w = iter->pending_moment ();
	}

      w = sneaky_insert_extra_moment (w);
      
      //      printf ("proccing %s\n ",       w.str().ch_C());


      
      if (first)
	{
	  first = false;
	  set_property ("measurePosition", w.smobbed_copy ());
	}

      prepare (w);
      if (iter->ok ())
	iter->process (w);
      
      one_time_step ();
    }
}
