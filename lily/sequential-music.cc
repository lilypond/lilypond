
#include "music-list.hh"
#include "sequential-music-iterator.hh"

Sequential_music::Sequential_music ()
  : Music_sequence ()
{
}


Moment
Sequential_music::get_length () const
{
  return Music_sequence::cumulative_length (get_mus_property ("elements"));
}
ADD_MUSIC (Sequential_music);

Moment
Sequential_music::start_mom () const
{
  return Music_sequence::first_start (get_mus_property ("elements"));
}
