/*   
  compressed-music-iterator.hh -- declare Compressed_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef COMPRESSED_MUSIC_ITERATOR_HH
#define COMPRESSED_MUSIC_ITERATOR_HH

#include "music-wrapper-iterator.hh"

class Compressed_music_iterator : public Music_wrapper_iterator
{
public:  
  // construction
protected:
  virtual void do_process_and_next (Moment);
};


#endif /* COMPRESSED_MUSIC_ITERATOR_HH */

