/*   
  repeated-music-iterator.hh -- declare Repeated_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef REPEATED_MUSIC_ITERATOR_HH
#define REPEATED_MUSIC_ITERATOR_HH

#include "music-wrapper-iterator.hh"
#include "voice-iterator.hh"

class Repeated_music_iterator : public virtual Music_wrapper_iterator, public virtual Sequential_music_iterator
{
public:
  Repeated_music_iterator();

protected:
  ~Repeated_music_iterator ();

  virtual Music_wrapper *music_wrapper_l () const;
  virtual Sequential_music* sequential_music_l() const;
  virtual bool ok () const;
  virtual void do_process_and_next (Moment);
  virtual void construct_children ();
  virtual void start_next_element ();
  virtual void leave_element ();
};


#endif /* REPEATED_MUSIC_ITERATOR_HH */

