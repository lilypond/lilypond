/*   
  request-iterator.hh -- declare Request_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef REQUEST_ITERATOR_HH
#define REQUEST_ITERATOR_HH

#include "music-iterator.hh"

class Simple_music_iterator : public Music_iterator
{
public:
protected:
  virtual void do_process_and_next (Moment );
};

#endif /* REQUEST_ITERATOR_HH */

