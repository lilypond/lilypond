/*   
  un-relativable-music.hh -- declare Un_relativable_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef UN_RELATIVABLE_MUSIC_HH
#define UN_RELATIVABLE_MUSIC_HH

#include "music-wrapper.hh"

class Un_relativable_music: public Music_wrapper
{
public:
  Un_relativable_music (SCM);
  VIRTUAL_COPY_CONSTRUCTOR (Music, Un_relativable_music);

  Pitch to_relative_octave (Pitch);
};

#endif /* UN_RELATIVABLE_MUSIC_HH */

