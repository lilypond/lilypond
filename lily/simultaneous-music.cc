/*   
  simultaneous-music.cc --  implement Simultaneous_music

  source file of the GNU LilyPond music typesetter

  (c) 1998--2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "input.hh"
#include "pitch.hh"
#include "music-list.hh"
#include "scm-option.hh"

Moment
Simultaneous_music::get_length () const
{
  return Music_sequence::maximum_length (get_property ("elements"));
}

Moment
Simultaneous_music::start_mom () const
{
  return Music_sequence::minimum_start (get_property ("elements"));
}

Simultaneous_music::Simultaneous_music (SCM x)
  : Music_sequence (x)
{

}

Pitch
Simultaneous_music::to_relative_octave (Pitch p)
{
  Pitch retval = music_list_to_relative (music_list (), p, false);
  if (lily_1_8_relative)
    {
      Pitch retval_1_8 = music_list_to_relative (music_list (), p, true);
      if (retval_1_8 != retval)
	lily_1_8_compatibility_used = true;

      retval = retval_1_8;
    }

  return retval;
}

ADD_MUSIC (Simultaneous_music);

Pitch
Event_chord::to_relative_octave (Pitch p)
{
  return music_list_to_relative (music_list (), p, true);
}

ADD_MUSIC (Event_chord);
