/*   
  simple-music-iterator.hh -- declare Simple_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#ifndef SIMPLE_MUSIC_ITERATOR_HH
#define SIMPLE_MUSIC_ITERATOR_HH

#include "music-iterator.hh"

class Simple_music_iterator : public Music_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  Simple_music_iterator ();
  Simple_music_iterator (Simple_music_iterator const &);
  virtual void do_process (Moment);
};

#endif /* SIMPLE_MUSIC_ITERATOR_HH */

