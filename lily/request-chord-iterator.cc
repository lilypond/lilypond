/*
  request-chord-iterator.cc -- implement Request_chord_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "debug.hh"
#include "request-chord-iterator.hh"
#include "music-list.hh"
#include "request.hh"



void
Request_chord_iterator::construct_children()
{
  elt_length_mom_ =elt_l ()->length_mom ();
  get_req_translator_l();
}

Request_chord*
Request_chord_iterator::elt_l () const
{
  return (Request_chord*) music_l_;
}

Request_chord_iterator::Request_chord_iterator ()
{
  last_b_ = false;
}


bool
Request_chord_iterator::ok() const
{
  return (elt_length_mom_ && !last_b_) || first_b_;
}

Moment
Request_chord_iterator::next_moment() const
{
  Moment m (0);
  if  (!first_b_)
    m = elt_length_mom_;
  return m;
}


void
Request_chord_iterator::do_print() const
{
#ifndef NPRINT
  DEBUG_OUT << "duration: " << elt_length_mom_;
#endif
}

void
Request_chord_iterator::do_process_and_next (Moment mom)
{
  if (first_b_)
    {
      for (SCM s = dynamic_cast<Music_sequence *> (music_l_)->music_list (); gh_pair_p (s);  s = gh_cdr (s))
	{
	  Music *mus = unsmob_music (gh_car (s));
	  if (Request * req_l = dynamic_cast<Request*> (mus))
	    {
	      bool gotcha = try_music (req_l);
	      if (!gotcha)
		req_l->origin ()->warning (_f ("Junking request: `%s'", classname( req_l)));
	    }
	  else
	    mus->origin ()->warning (_f ("Huh?  Not a Request: `%s'",
						 classname (mus)));
		    }
      first_b_ = false;
    }

  if (mom >= elt_length_mom_)
    last_b_ = true;
}
