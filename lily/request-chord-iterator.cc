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
}

Request_chord_iterator::Request_chord_iterator (Request_chord_iterator const &src)
  : Simple_music_iterator (src)
{
}

Translator_group*
Request_chord_iterator::get_req_translator_l ()
{
  assert (report_to_l ());
  if (report_to_l ()->is_bottom_translator_b ())
    return report_to_l ();

  set_translator (report_to_l ()->get_default_interpreter ());
  return report_to_l ();
}

void
Request_chord_iterator::construct_children()
{
  Simple_music_iterator::construct_children ();
  get_req_translator_l();
}

Request_chord*
Request_chord_iterator::elt_l () const
{
  return (Request_chord*) music_l_;
}


SCM
Request_chord_iterator::get_music (Moment)const
{
  SCM s = SCM_EOL;
  if (music_l_)
    {
      Music_sequence * ms = dynamic_cast<Music_sequence*> (music_l_);
     
      for (SCM m = ms->music_list (); gh_pair_p (m); m = gh_cdr (m))
	{
	  s = gh_cons (gh_car (m) , s);
	}
    }
  return s;
}

void
Request_chord_iterator::process (Moment m)
{
  last_processed_mom_ = m;
  if (music_l_)
    {
      for (SCM s = dynamic_cast<Music_sequence *> (music_l_)->music_list ();
	   gh_pair_p (s);  s = gh_cdr (s))
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

     music_l_ =0;
    }
}
