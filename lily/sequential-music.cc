#include "music-list.hh"
#include "sequential-music-iterator.hh"

Sequential_music::Sequential_music (SCM x)
  : Music_sequence (x)
{
}

ADD_MUSIC (Sequential_music);

Moment
Sequential_music::start_mom () const
{
  return Music_sequence::first_start (get_property ("elements"));
}
