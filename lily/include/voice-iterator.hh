/*
  Sequential_music-iter.hh -- declare Sequential_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>

*/

#ifndef SEQUENTIAL_MUSIC_ITER_HH
#define SEQUENTIAL_MUSIC_ITER_HH

#include "music-iterator.hh"
#include "pcursor.hh"

class Sequential_music_iterator :  public Music_iterator
{
public:
  Sequential_music_iterator ();
  
protected:
  virtual ~Sequential_music_iterator();    

  virtual Sequential_music* sequential_music_l() const;
  virtual void do_print() const;
  virtual void construct_children();
  virtual void do_process_and_next (Moment);
  virtual Moment next_moment() const;
  virtual bool ok() const;
  virtual void start_next_element();
  virtual void leave_element();

private:
  Moment here_mom_;
  PCursor<Music*> *cursor_p_;
  Music_iterator * iter_p_;
  void set_Sequential_music_translator();
};

#endif // SEQUENTIAL_MUSIC_ITER_HH
