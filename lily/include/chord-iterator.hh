/*
  chord-iter.hh -- declare Chord_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Simultaneous_music_ITER_HH
#define Simultaneous_music_ITER_HH


#include "music-iterator.hh"
#include "plist.hh"

class Simultaneous_music_iterator : public Music_iterator
{
  const Simultaneous_music *simultaneous_music_C_;
  Pointer_list<Music_iterator*> children_p_list_;
public:
  ~Simultaneous_music_iterator();
  Simultaneous_music_iterator (Simultaneous_music const*);
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual void do_print() const;
  virtual void construct_children();
  virtual void process_and_next (Moment);
  virtual Moment next_moment() const;
  virtual bool ok() const;
};

#endif // Simultaneous_music_ITER_HH
