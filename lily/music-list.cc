/*
  music-list.cc -- implement Music_sequence, Simultaneous_music, Sequential_music

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-list.hh"
#include "music-wrapper.hh"
#include "musical-pitch.hh"
#include "request.hh"
#include "musical-request.hh"
#include "music-iterator.hh"
#include "main.hh"
#include "killing-cons.tcc"

Moment
Simultaneous_music::length_mom () const
{
  return maximum_length ();
}

Simultaneous_music::Simultaneous_music(SCM head)
  : Music_sequence (head)
{
  set_mus_property ("type", ly_symbol2scm ("simultaneous-music"));
}

Sequential_music::Sequential_music(SCM head)
  : Music_sequence (head)
{
  set_mus_property ("type", ly_symbol2scm ("sequential-music"));
}


Moment
Sequential_music::length_mom () const
{
  return cumulative_length ();
}

Musical_pitch
Simultaneous_music::to_relative_octave (Musical_pitch p)
{
  return do_relative_octave (p, true);
}

Request_chord::Request_chord(SCM s)
  : Simultaneous_music (s)
{
  set_mus_property ("type", ly_symbol2scm ("request-chord"));
}

Musical_pitch
Request_chord::to_relative_octave (Musical_pitch last)
{
  for (SCM s = music_list (); gh_pair_p (s);  s = gh_cdr (s))
    {
      Music * mus = unsmob_music (gh_car (s));
      if (Melodic_req *m= dynamic_cast <Melodic_req *> (mus))
	{
	  Musical_pitch &pit = m->pitch_;
	  pit.to_relative_octave (last);
	  return pit;
	}
    }
  return last;
}



