/*   
  transposed-music.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef TRANSPOSED_MUSIC_HH
#define TRANSPOSED_MUSIC_HH


#include "music-wrapper.hh"

class Transposed_music : public Music_wrapper
{
public:
  Musical_pitch transpose_to_pitch_;

  void do_print () const;
  Transposed_music (Music*, Musical_pitch);
  
  VIRTUAL_COPY_CONS(Music);
  virtual Musical_pitch to_relative_octave (Musical_pitch);
};

#endif /* TRANSPOSED_MUSIC_HH */

