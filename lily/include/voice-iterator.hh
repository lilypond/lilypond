/*
  voice-iter.hh -- declare Voice_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef VOICE_ITER_HH
#define VOICE_ITER_HH


#include "music-iterator.hh"
#include "pcursor.hh"

class Voice_iterator :  private PCursor<Music*>, public Music_iterator
{
  Moment here_mom_;
  const Voice * voice_C_;
  Music_iterator * iter_p_;
  void start_next_element();
  void leave_element();
  void set_voice_translator();
    
public:
  Voice_iterator (Voice const*);
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual void do_print() const;
  virtual void construct_children();
  ~Voice_iterator();    
  virtual void process_and_next (Moment);
  virtual Moment next_moment() const;
  virtual bool ok() const;
};

#endif // VOICE_ITER_HH
