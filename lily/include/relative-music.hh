/*   
  relative-music.hh -- declare Relative_octave_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#ifndef RELATIVE_MUSIC_HH
#define RELATIVE_MUSIC_HH

#include "music-wrapper.hh"

class Relative_octave_music : public Music_wrapper
{
public:
  Musical_pitch last_pitch_;

  void do_print () const;
  Relative_octave_music (Music*, Musical_pitch);
  DECLARE_MY_RUNTIME_TYPEINFO;
  VIRTUAL_COPY_CONS (Relative_octave_music, Music);
  virtual Musical_pitch to_relative_octave (Musical_pitch);
};


#endif /* RELATIVE_MUSIC_HH */

