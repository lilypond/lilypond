/*   
  relative-music.cc --  implement Relative_octave_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "relative-music.hh"
#include "warn.hh"
#include "scm-option.hh"


Pitch
Relative_octave_music::to_relative_octave (Pitch p)
{
  if (lily_1_8_relative)
    {
      lily_1_8_compatibility_used = true;
      /* ugh: last-pitch should  be junked.

         Change this for lilypond 2.0.

	 FIXME: change WHAT?  We're at 2.3 already -- jcn

	 When you do, B should start where A left off.

	\relative { A \relative { ...} B }  */
      return *unsmob_pitch (get_property ("last-pitch"));
    }
  else
    return p;
}


Relative_octave_music::Relative_octave_music ()
{
}

ADD_MUSIC (Relative_octave_music);


