
#include "music-list.hh"
#include "sequential-music-iterator.hh"

Sequential_music::Sequential_music (SCM head)
  : Music_sequence (head)
{
  set_mus_property ("iterator-ctor",
		    Sequential_music_iterator::constructor_proc);
}
Sequential_music::Sequential_music ()
  : Music_sequence ()
{
  set_mus_property ("iterator-ctor",
		    Sequential_music_iterator::constructor_proc);
}


Moment
Sequential_music::length_mom () const
{
  return Music_sequence::cumulative_length (get_mus_property ("elements"));
}
ADD_MUSIC (Sequential_music);

Moment
Sequential_music::start_mom () const
{
  return Music_sequence::first_start (get_mus_property ("elements"));
}
