/*   
  music-wrapper-iterator.hh -- declare Music_wrapper_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef MUSIC_WRAPPER_ITERATOR_HH
#define MUSIC_WRAPPER_ITERATOR_HH

#include "music.hh"
#include "music-iterator.hh"

/** 
  The iterator for a #Music_wrapper#.  Since #Music_wrapper# essentially
  does nothing, this iterator creates a child iterator and delegates
  all work to that child.
 */
class Music_wrapper_iterator : public Music_iterator
{
public:
  Music_wrapper_iterator ();
  ~Music_wrapper_iterator ();

  virtual void construct_children  () ;
  virtual Moment next_moment () const;
  virtual bool ok () const;

protected:
  virtual void do_print () const;
  virtual void do_process_and_next (Moment) ;
  virtual Music_iterator *try_music_in_children (Music const *) const;

  Music_iterator *child_iter_p_;
};

#endif /* MUSIC_WRAPPER_ITERATOR_HH */



