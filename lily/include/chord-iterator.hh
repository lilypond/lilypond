/*
  chord-iter.hh -- declare Chord_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef CHORD_ITER_HH
#define CHORD_ITER_HH


#include "music-iterator.hh"
#include "plist.hh"

class Chord_iterator : public Music_iterator
{
  const Chord *chord_C_;
  Pointer_list<Music_iterator*> children_p_list_;
public:
  ~Chord_iterator();
  Chord_iterator (Chord const*);
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual void do_print() const;
  virtual void construct_children();
  virtual void process_and_next (Moment);
  virtual Moment next_moment() const;
  virtual bool ok() const;
};

#endif // CHORD_ITER_HH
