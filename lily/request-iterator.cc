/*
  request-iterator.cc -- implement Request_chord_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "debug.hh"
#include "request-iterator.hh"
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
  DOUT << "duration: " << elt_length_mom_;
#endif
}

void
Request_chord_iterator::do_process_and_next (Moment mom)
{
  if (first_b_)
    {
      for (PCursor<Music*> i (elt_l ()->music_p_list_p_->top ()); i.ok(); i++)
	{
	  if (Request * req_l = dynamic_cast<Request*> (i.ptr()))
	    {
	      bool gotcha = report_to_l()->try_music (req_l);
	      if (!gotcha)
		req_l->warning (_f ("junking request: `%s\'", classname( req_l)));
	    }
	  else
	    i.ptr ()->warning (_f ("Huh? Not a Request: `%s\'",
				   classname (i.ptr())));
	}
      first_b_ = false;
    }

  if (mom >= elt_length_mom_)
    last_b_ = true;
}
