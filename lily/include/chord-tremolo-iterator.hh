/*   
  chord-tremolo-iterator.hh -- declare Chord_tremolo_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef CHORD_TREMOLO_ITERATOR_HH
#define CHORD_TREMOLO_ITERATOR_HH

#include "music-iterator.hh"

class Chord_tremolo_iterator : public Music_iterator
{
  Moment factor_;
  Music_iterator * child_iter_p_;

public:
  Chord_tremolo_iterator ();

protected:
  virtual ~Chord_tremolo_iterator ();
  virtual Moment next_moment () const;
  virtual void construct_children  () ;
  virtual bool ok () const;
  virtual void do_print () const;
  virtual void do_process_and_next (Moment) ;
  virtual Music_iterator *try_music_in_children (Music const *) const;
};


#endif /* CHORD_TREMOLO_ITERATOR_HH */

