/*   
  grace-iterator.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef GRACE_ITERATOR_HH
#define GRACE_ITERATOR_HH

#include "music-wrapper-iterator.hh"

class Grace_iterator : public Music_wrapper_iterator
{
public:
  ~Grace_iterator ();
  virtual void construct_children () ;
  virtual void do_process_and_next (Moment);
  Moment next_moment () const;
  Music* next_music_l ();
};



#endif /* GRACE_ITERATOR_HH */


