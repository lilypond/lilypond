/*   
  untransposable-music.hh -- declare Untransposable_music

  source file of the GNU LilyPond music typesetter

  (c) 2001--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef UNTRANSPOSABLE_MUSIC_HH
#define UNTRANSPOSABLE_MUSIC_HH

#include "music-wrapper.hh"

class Untransposable_music : public Music_wrapper
{
public:
  Untransposable_music (SCM);
  VIRTUAL_COPY_CONSTRUCTOR (Music, Untransposable_music);

  virtual Pitch to_relative_octave (Pitch);
};


#endif /* UNTRANSPOSABLE_MUSIC_HH */

