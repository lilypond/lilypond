/*   
  folded-repeat-iterator.hh -- declare Folded_repeat_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FOLDED_REPEAT_ITERATOR_HH
#define FOLDED_REPEAT_ITERATOR_HH

#include "music-iterator.hh"

/**
   Iterate through a repeated section: first do the body, then
   all alternatives in parallel.
 */
class Folded_repeat_iterator : public Music_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  static SCM constructor_cxx_function;

  Folded_repeat_iterator (Folded_repeat_iterator const &src);
  Folded_repeat_iterator ();
  ~Folded_repeat_iterator ();
  
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual bool ok () const;

protected:
  void enter_alternative ();
  void leave_body ();
  
  virtual void process (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;

private:
  Music_iterator * main_iter_p_;
  Music_iterator * alternative_iter_p_;

  Moment main_length_mom_;
};
#endif /* FOLDED_REPEAT_ITERATOR_HH */

