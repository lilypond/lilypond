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
  DECLARE_SCHEME_CALLBACK(constructor, ());
  Percent_repeat_iterator ();
  Percent_repeat_iterator (Percent_repeat_iterator const & );
protected:
  virtual void derived_mark () const;
  virtual Moment pending_moment () const;
  virtual void do_quit(); 
  virtual void construct_children () ;
  virtual bool ok () const;
  virtual void process (Moment) ;
  virtual Music_iterator *try_music_in_children (Music *) const;

private:
  Music_iterator * child_iter_;
  Moment finish_mom_;
};


#endif /* PERCENT_REPEAT_ITERATOR_HH */
