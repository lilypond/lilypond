/*
  simultaneous-music-iterator.hh -- declare Simultaneous_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SIMULTANEOUS_MUSIC_ITERATOR_HH
#define SIMULTANEOUS_MUSIC_ITERATOR_HH

#include "cons.hh"
#include "music-list-iterator.hh"

class Simultaneous_music_iterator : public Music_list_iterator
{
public:

  /// make a new context for every child.
  bool separate_contexts_b_;
  
  Simultaneous_music_iterator ();
  virtual ~Simultaneous_music_iterator ();

  virtual void construct_children ();
  virtual Moment next_moment () const;
  virtual bool ok () const;

protected:
  virtual void do_print () const;
  virtual void do_process_and_next (Moment);

private:
  Cons_list<Music_iterator> children_p_list_;
};

#endif // SIMULTANEOUS_MUSIC_ITERATOR_HH
