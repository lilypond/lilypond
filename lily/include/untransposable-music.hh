
/*   
untransposable-music.hh -- declare 

source file of the GNU LilyPond music typesetter

(c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef UNTRANSPOSABLE_MUSIC_HH
#define UNTRANSPOSABLE_MUSIC_HH
#include "music-wrapper.hh"

class Untransposable_music : public Music_wrapper
{
public:
  Untransposable_music ();
  virtual Pitch to_relative_octave (Pitch);
  virtual void transpose (Pitch); 
  VIRTUAL_COPY_CONS(Music);
};


#endif /* UNTRANSPOSABLE_MUSIC_HH */

