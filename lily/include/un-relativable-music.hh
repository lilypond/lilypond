/*   
  un-relativable-music.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef UN_RELATIVABLE_MUSIC_HH
#define UN_RELATIVABLE_MUSIC_HH

#include "music-wrapper.hh"

class Un_relativable_music: public Music_wrapper
{
public:
  Un_relativable_music ();
  Pitch to_relative_octave (Pitch);
  VIRTUAL_COPY_CONS(Music);
};


#endif /* UN_RELATIVABLE_MUSIC_HH */

