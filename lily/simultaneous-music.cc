
#include "moment.hh"
#include "pitch.hh"
#include "simultaneous-music-iterator.hh"
#include "music-list.hh"

Moment
Simultaneous_music::length_mom () const
{
  return Music_sequence::maximum_length (get_mus_property ("elements"));
}

Moment
Simultaneous_music::start_mom () const
{
  return Music_sequence::minimum_start (get_mus_property ("elements"));
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

Pitch
Simultaneous_music::to_relative_octave (Pitch p)
{
  return do_relative_octave (p, true);
}
ADD_MUSIC (Simultaneous_music);
