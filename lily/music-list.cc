/*
  music-list.cc -- implement Music_sequence, Simultaneous_music, Sequential_music

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-list.hh"
#include "music-wrapper.hh"
#include "pitch.hh"
#include "request.hh"
#include "musical-request.hh"
#include "music-iterator.hh"
#include "main.hh"
#include "killing-cons.tcc"
#include "simultaneous-music-iterator.hh"
#include "sequential-music-iterator.hh"
#include "request-chord-iterator.hh"

Moment
Simultaneous_music::length_mom () const
{
  return maximum_length ();
}

Simultaneous_music::Simultaneous_music (SCM head)
  : Music_sequence (head)
{
  set_mus_property ("iterator-ctor",
		    Simultaneous_music_iterator::constructor_cxx_function);
}

Simultaneous_music::Simultaneous_music ()
  : Music_sequence ()
{
  set_mus_property ("iterator-ctor",
		    Simultaneous_music_iterator::constructor_cxx_function);
  
}

Sequential_music::Sequential_music (SCM head)
  : Music_sequence (head)
{
  set_mus_property ("iterator-ctor",
		    Sequential_music_iterator::constructor_cxx_function);
}
Sequential_music::Sequential_music ()
  : Music_sequence ()
{
  set_mus_property ("iterator-ctor",
		    Sequential_music_iterator::constructor_cxx_function);
}


Moment
Sequential_music::length_mom () const
{
  return cumulative_length ();
}

Pitch
Simultaneous_music::to_relative_octave (Pitch p)
{
  return do_relative_octave (p, true);
}

Request_chord::Request_chord (SCM s)
  : Simultaneous_music (s)
{
  set_mus_property ("iterator-ctor",
		    Request_chord_iterator::constructor_cxx_function);
}

Request_chord::Request_chord ()
{
  set_mus_property ("iterator-ctor",
		    Request_chord_iterator::constructor_cxx_function);
}

Pitch
Request_chord::to_relative_octave (Pitch last)
{
  for (SCM s = music_list (); gh_pair_p (s);  s = gh_cdr (s))
    {
      Music * mus = unsmob_music (gh_car (s));
      if (Melodic_req *m= dynamic_cast <Melodic_req *> (mus))
	{
	  Pitch pit = *unsmob_pitch (m->get_mus_property ("pitch"));
	  
	  pit.to_relative_octave (last);
	  m->set_mus_property ("pitch", pit.smobbed_copy ());
	  	  
	  return pit;
	}
    }
  return last;
}



ADD_MUSIC (Simultaneous_music);
ADD_MUSIC (Sequential_music);
ADD_MUSIC (Request_chord);
