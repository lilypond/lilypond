/*
  simultaneous-music-iterator.hh -- declare Simultaneous_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SIMULTANEOUS_MUSIC_ITERATOR_HH
#define SIMULTANEOUS_MUSIC_ITERATOR_HH
#include "music-iterator.hh"
#include "cons.hh"

class Simultaneous_music_iterator : public Music_iterator
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
  virtual Music_iterator *try_music_in_children (Music const*) const;


private:
  Cons_list<Music_iterator> children_p_list_;
};

#endif // SIMULTANEOUS_MUSIC_ITERATOR_HH
