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

Request_chord_iterator::Request_chord_iterator ()
{
  last_b_ = false;
}

Request_chord_iterator::Request_chord_iterator (Request_chord_iterator const &src)
  : Music_iterator (src)
{
  last_b_ = src.last_b_;
  elt_length_mom_ = src.elt_length_mom_;
}

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

bool
Request_chord_iterator::next ()
{
  if (first_b_)
    first_b_ = false;
  else
    last_b_ = true;
  return ok ();
}

void
Request_chord_iterator::do_process_and_next (Moment)
{
  // URG
  if (first_b_)
    {
      for (SCM s = dynamic_cast<Music_sequence *> (get_music ())->music_list (); gh_pair_p (s);  s = gh_cdr (s))
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
    }

  next ();
}
