/*   
  repeated-music-iterator.hh -- declare Repeated_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef REPEATED_MUSIC_ITERATOR_HH
#define REPEATED_MUSIC_ITERATOR_HH

#include "music-wrapper-iterator.hh"
#include "sequential-music-iterator.hh"

class Repeated_music_iterator : public Music_iterator
{
public:
  Repeated_music_iterator ();
  ~Repeated_music_iterator ();

  virtual void construct_children ();
  virtual Moment next_moment () const;
  virtual bool ok () const;

protected:
  virtual Repeated_music* repeated_music_l () const;
  virtual void do_print () const;
  virtual void do_process_and_next (Moment);

private:
  Music_iterator* repeat_iter_p_;
  Sequential_music_iterator* alternative_iter_p_;
};

#endif /* REPEATED_MUSIC_ITERATOR_HH */

