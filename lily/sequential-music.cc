
#include "music-list.hh"
#include "sequential-music-iterator.hh"

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
ADD_MUSIC (Sequential_music);

Moment
Sequential_music::start_mom () const
{
  return first_start ();
}
