/*   
  repeated-music-iterator.hh -- declare Repeated_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef REPEATED_MUSIC_ITERATOR_HH
#define REPEATED_MUSIC_ITERATOR_HH

#include "music-list-iterator.hh"

class Repeated_music_iterator : public Music_list_iterator
{
public:
  Repeated_music_iterator ();
  virtual ~Repeated_music_iterator ();

  virtual void construct_children ();
  virtual Moment next_moment () const;
  virtual bool ok () const;

protected:
  virtual void do_print () const;
  virtual void do_process_and_next (Moment);
private:
  void start_next_element ();

  int unfold_i_;
  Moment here_mom_;
  Music_iterator* repeat_iter_p_;
  Music_list_iterator* alternative_iter_p_;
};

#endif /* REPEATED_MUSIC_ITERATOR_HH */

