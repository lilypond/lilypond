/*
  relative-music.cc -- implement Relative_octave_music

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "music.hh"
#include "warn.hh"
#include "program-option.hh"

class Relative_octave_music
{
public:
  DECLARE_SCHEME_CALLBACK (relative_callback, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (no_relative_callback, (SCM, SCM));
};

MAKE_SCHEME_CALLBACK (Relative_octave_music, no_relative_callback, 2)
  SCM
Relative_octave_music::no_relative_callback (SCM /* music */,
					     SCM pitch)
{
  return pitch;
}

MAKE_SCHEME_CALLBACK (Relative_octave_music, relative_callback, 2)
  SCM
Relative_octave_music::relative_callback (SCM music, SCM pitch)
{
  Music *me = unsmob_music (music);
  if (lily_1_8_relative)
    {
      lily_1_8_compatibility_used = true;
      /*  last-pitch should be junked some time, when
	  we ditch 1.8 compat too.

	  When you do, B should start where A left off.

	  \relative { A \relative { ...} B }  */
      SCM last_pitch = me->get_property ("last-pitch");
      Pitch *ptr = unsmob_pitch (last_pitch);
      return (ptr) ? last_pitch : pitch;
    }
  else
    return pitch;
}

