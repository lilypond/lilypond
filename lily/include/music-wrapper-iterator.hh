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
public:
  Music_wrapper_iterator ();

protected:
  virtual ~Music_wrapper_iterator ();

  virtual void do_print () const;
  virtual void construct_children  () ;
  virtual void do_process_and_next (Moment) ;
  virtual Moment next_moment () const;

protected:
  virtual Music_wrapper *music_wrapper_l () const;
  virtual bool ok () const;

private:
  Music_iterator *child_iter_p_;
};

#endif /* MUSIC_WRAPPER_ITERATOR_HH */



