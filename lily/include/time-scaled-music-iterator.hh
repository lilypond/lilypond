/*   
  compressed-music-iterator.hh -- declare Time_scaled_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef Time_scaled_music_ITERATOR_HH
#define Time_scaled_music_ITERATOR_HH

#include "music-wrapper-iterator.hh"

class Time_scaled_music_iterator : public Music_wrapper_iterator
{
public:  
  // construction
protected:
  virtual void do_process_and_next (Moment);
};


#endif /* Time_scaled_music_ITERATOR_HH */

