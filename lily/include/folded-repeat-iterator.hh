/*
  folded-repeat-iterator.hh -- declare Folded_repeat_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  DECLARE_SCHEME_CALLBACK (constructor, ());
  DECLARE_CLASSNAME(Folded_repeat_iterator);

  Folded_repeat_iterator (Folded_repeat_iterator const &src);
  Folded_repeat_iterator ();
  virtual void derived_mark () const;
  virtual void derived_substitute (Context *f, Context *t);

  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual bool ok () const;

protected:
  void enter_alternative ();
  void leave_body ();

  virtual void process (Moment);

private:
  Music_iterator *main_iter_;
  Music_iterator *alternative_iter_;

  Moment main_length_mom_;
};
#endif /* FOLDED_REPEAT_ITERATOR_HH */

