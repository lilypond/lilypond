/*
  Sequential_music-iterator.hh -- declare Sequential_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef SEQUENTIAL_MUSIC_ITERATOR_HH
#define SEQUENTIAL_MUSIC_ITERATOR_HH

#include "music-list-iterator.hh"

/** Sequential_music iteration: walk each element in turn, and
  construct an iterator for every element.
  
 */
class Sequential_music_iterator :  public Music_list_iterator
{
public:
  Sequential_music_iterator ();
  virtual ~Sequential_music_iterator ();

  virtual void construct_children ();
  virtual Moment next_moment () const;
  virtual bool ok () const;

protected:
  Sequential_music* sequential_music_l () const;

  virtual void do_print() const;
  virtual void do_process_and_next (Moment);

private:
  Moment here_mom_;
  PCursor<Music*> *cursor_p_;
  Music_iterator * iter_p_;

  /*
    perhaps these can be virtual and protected iso. private?  
   */
  void start_next_element();
  void leave_element();
  void set_Sequential_music_translator();
};

#endif // SEQUENTIAL_MUSIC_ITERATOR_HH
