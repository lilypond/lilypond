/*
  Sequential_music-iter.hh -- declare Sequential_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Sequential_music_ITER_HH
#define Sequential_music_ITER_HH


#include "music-iterator.hh"
#include "pcursor.hh"

class Sequential_music_iterator :  public Music_iterator
{
  Moment here_mom_;
  PCursor<Music*> *cursor_p_;
  Music_iterator * iter_p_;
  void start_next_element();
  void leave_element();
  void set_Sequential_music_translator();
protected:
  Sequential_music * sequential_music_l() const;
public:
  Sequential_music_iterator ();
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual void do_print() const;
  virtual void construct_children();
  ~Sequential_music_iterator();    
  virtual void do_process_and_next (Moment);
  virtual Moment next_moment() const;
  virtual bool ok() const;
};

#endif // Sequential_music_ITER_HH
