/*
  global-translator.cc -- implement Global_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

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

  if (m < now_mom_ )
    programming_error ("Trying to freeze in time.");
  
  for (int i=0; i <  extra_mom_pq_.size(); i++)
    if (extra_mom_pq_[i] == m)
      return;
  extra_mom_pq_.insert (m);
}

Moment
Global_translator::sneaky_insert_extra_moment (Moment w)
{
  while (extra_mom_pq_.size() && extra_mom_pq_.front() <= w)
    w = extra_mom_pq_.get();
  return w;
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
  bool first = true;
  while (iter->ok () || moments_left_i ())
    {
      Moment w;
      w.set_infinite (1);
      if (iter->ok ())
	{
	  w = iter->next_moment();

	  if (!first)
	    iter->next ();
	  else
	    first = false;
      
	  DEBUG_OUT << "proccing: " << w << '\n';
	  if (flower_dstream && !flower_dstream->silent_b ("walking"))
	    iter->print();
	}

      w = sneaky_insert_extra_moment (w);
      prepare (w);
      
      if (flower_dstream && !flower_dstream->silent_b ("walking"))
	print();

      iter->process (w);

      /*
	Urg, this is request_chord_iterator.
       */
      for (SCM i = iter->get_music (); gh_pair_p (i); i = SCM_CDR (i))
	{
	  assert (gh_pair_p (i));
	  SCM p = SCM_CAR (i);
	  Music *m = unsmob_music (SCM_CAR (p));
	  Translator *t = unsmob_translator (SCM_CDR (p));
	  assert (m);
	  assert (t);
	  for (SCM s = dynamic_cast<Music_sequence *> (m)->music_list ();
	       gh_pair_p (s);  s = SCM_CDR (s))
	    {
	      Music *m = unsmob_music (SCM_CAR (s));
	      if (Request* r = dynamic_cast<Request*> (m))
		{
		  bool b = t->try_music (m);
		  if (!b)
		    m->origin ()->warning (_f ("Junking request: `%s'",
					       classname(m)));
		}
	      else
		m->origin ()->warning (_f ("Huh?  Not a Request: `%s'",
					   classname (m)));
	    }
	  if (! dynamic_cast<Music_sequence *> (m))
	    {
	      bool b = t->try_music (m);
	      if (!b)
		{
		  /*
		    Children?
		  */
		  printf ("junking:\n");
		  m->print ();
		  t->print ();
		}
	    }
	}
      
      process ();
    }
}
