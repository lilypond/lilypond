/*
  simultaneous-music-iterator.hh -- declare Simultaneous_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SIMULTANEOUS_MUSIC_ITERATOR_HH
#define SIMULTANEOUS_MUSIC_ITERATOR_HH

#include "music-iterator.hh"

class Simultaneous_music_iterator : public Music_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  Simultaneous_music_iterator ();
  Simultaneous_music_iterator (Simultaneous_music_iterator const&);
  virtual void derived_mark () const;
  DECLARE_SCHEME_CALLBACK(constructor, ());
  
  /// make a new context for every child.
  bool separate_contexts_b_;
  int cursor_;

  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit(); 
  virtual bool ok () const;
  virtual SCM get_pending_events (Moment)const;
  virtual void skip (Moment);

protected:
  virtual void process (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;

private:
  SCM children_list_;
};

#endif // SIMULTANEOUS_MUSIC_ITERATOR_HH
