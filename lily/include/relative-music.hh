/*   
  relative-music.hh -- declare Relative_octave_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef RELATIVE_MUSIC_HH
#define RELATIVE_MUSIC_HH

#include "music-wrapper.hh"

class Relative_octave_music : public Music_wrapper
{
public:
  Relative_octave_music (SCM l);
  Relative_octave_music ();
  
  VIRTUAL_COPY_CONS (Music);
  virtual Pitch to_relative_octave (Pitch);
};


#endif /* RELATIVE_MUSIC_HH */

