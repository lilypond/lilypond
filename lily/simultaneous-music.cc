#include "input.hh"
#include "moment.hh"
#include "pitch.hh"
#include "music-list.hh"

Moment
Simultaneous_music::get_length () const
{
  return Music_sequence::maximum_length (get_mus_property ("elements"));
}

Moment
Simultaneous_music::start_mom () const
{
  return Music_sequence::minimum_start (get_mus_property ("elements"));
}

Simultaneous_music::Simultaneous_music()
{

}

Pitch
Simultaneous_music::to_relative_octave (Pitch p)
{
  return music_list_to_relative (music_list(),p, lily_1_8_relative);
}

ADD_MUSIC (Simultaneous_music);

Pitch
Event_chord::to_relative_octave (Pitch p)
{
  return music_list_to_relative (music_list (), p, true);
}

ADD_MUSIC(Event_chord);
