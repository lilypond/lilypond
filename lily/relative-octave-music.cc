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
      /*  last-pitch should be junked some time, when
	  we ditch 1.8 compat too.

	 When you do, B should start where A left off.

	\relative { A \relative { ...} B }  */
      Pitch *ptr = unsmob_pitch (get_property ("last-pitch"));
      return (ptr) ?  *ptr : p;
    }
  else
    return p;
}


Relative_octave_music::Relative_octave_music (SCM x)
  : Music_wrapper (x)
{
  
}

ADD_MUSIC (Relative_octave_music);


