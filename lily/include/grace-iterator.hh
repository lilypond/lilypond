/*   
  grace-iterator.hh -- declare Grace_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef NEWGRACE_ITERATOR_HH
#define NEWGRACE_ITERATOR_HH

#include "music-wrapper-iterator.hh"

class Grace_iterator : public Music_wrapper_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  virtual void process (Moment);
  DECLARE_SCHEME_CALLBACK(constructor, ());
  Moment pending_moment () const;
};



#endif /* GRACE_ITERATOR_HH */


