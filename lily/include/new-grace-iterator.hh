/*   
  new-grace-iterator.hh -- declare New-Grace_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef NEWGRACE_ITERATOR_HH
#define NEWGRACE_ITERATOR_HH

#include "music-wrapper-iterator.hh"

class New_grace_iterator : public Music_wrapper_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  ~New_grace_iterator ();
  virtual void construct_children () ;
  virtual void process (Moment);
  static SCM constructor_cxx_function;
  Moment pending_moment () const;
};



#endif /* GRACE_ITERATOR_HH */


