/*
  request-chord-iterator.cc -- implement Request_chord_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
Request_chord_iterator::construct_children ()
{
  Simple_music_iterator::construct_children ();
  get_req_translator_l ();
}

Request_chord*
Request_chord_iterator::elt_l () const
{
  return (Request_chord*) music_l ();
}

SCM
Request_chord_iterator::get_music (Moment) const
{
  SCM s = SCM_EOL;
  if (last_processed_mom_ < Moment (0))
    {
      Music_sequence * ms = dynamic_cast<Music_sequence*> (music_l ());
     
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
  if (last_processed_mom_ < Moment (0))
    {
      for (SCM s = dynamic_cast<Music_sequence *> (music_l ())->music_list ();
	   gh_pair_p (s);  s = gh_cdr (s))
	{
	  Music *mus = unsmob_music (gh_car (s));

	  bool gotcha = try_music (mus);
	  if (!gotcha)
	    mus->origin ()->warning (_f ("Junking request: `%s'", classname (mus)));
	}
    }
  skip (m);
}

IMPLEMENT_CTOR_CALLBACK (Request_chord_iterator);
