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
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  static SCM constructor_cxx_function;
  Chord_tremolo_iterator ();
  Chord_tremolo_iterator (Chord_tremolo_iterator const & );
protected:
  virtual ~Chord_tremolo_iterator ();
  virtual Moment pending_moment () const;
  virtual void construct_children  () ;
  virtual bool ok () const;
  virtual void process (Moment) ;
  virtual Music_iterator *try_music_in_children (Music *) const;

private:
  Moment factor_;
  Music_iterator * child_iter_p_;
};


#endif /* CHORD_TREMOLO_ITERATOR_HH */

