/*   
  music-wrapper-iterator.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef MUSIC_WRAPPER_ITERATOR_HH
#define MUSIC_WRAPPER_ITERATOR_HH

#include "music.hh"
#include "music-iterator.hh"

class Music_wrapper_iterator : public Music_iterator
{
  Music_iterator *child_iter_p_;
  Music_wrapper *music_wrapper_l();
public:
  Music_wrapper_iterator ();
  ~Music_wrapper_iterator ();
  virtual void do_print () const;
  virtual void construct_children  () ;
  virtual void do_process_and_next (Moment) ;
  virtual Moment next_moment () const;
  virtual bool ok () const;
  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif /* MUSIC_WRAPPER_ITERATOR_HH */



