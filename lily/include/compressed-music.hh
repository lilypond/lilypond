/*   
  compressed-music.hh -- declare Compressed_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef COMPRESSED_MUSIC_HH
#define COMPRESSED_MUSIC_HH

#include "music-wrapper.hh"
/**
   Tempo expansion or compression.
 */
class Compressed_music : public Music_wrapper
{
public:
  int num_i_;
  int den_i_;

  Compressed_music (int, int, Music *);
  DECLARE_MY_RUNTIME_TYPEINFO;
  VIRTUAL_COPY_CONS (Compressed_music, Music);
};

#endif /* COMPRESSED_MUSIC_HH */

