/*   
  percent-repeat-iterator.hh -- declare Percent_repeat_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef PERCENT_REPEAT_ITERATOR_HH
#define PERCENT_REPEAT_ITERATOR_HH

#include "music-iterator.hh"

class Percent_repeat_iterator : public Music_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  static SCM constructor_cxx_function;
  Percent_repeat_iterator ();
  Percent_repeat_iterator (Percent_repeat_iterator const & );
protected:
  virtual ~Percent_repeat_iterator ();
  virtual Moment pending_moment () const;
  virtual void construct_children () ;
  virtual bool ok () const;
  virtual void process (Moment) ;
  virtual Music_iterator *try_music_in_children (Music *) const;

private:
  Music_iterator * child_iter_p_;
  Moment finish_mom_;
};


#endif /* PERCENT_REPEAT_ITERATOR_HH */
