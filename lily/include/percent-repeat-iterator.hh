/*
  percent-repeat-iterator.hh -- declare Percent_repeat_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2001--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PERCENT_REPEAT_ITERATOR_HH
#define PERCENT_REPEAT_ITERATOR_HH

#include "music-iterator.hh"

class Percent_repeat_iterator : public Music_iterator
{
public:
  DECLARE_CLASSNAME(Percent_repeat_iterator);
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Percent_repeat_iterator ();
protected:
  virtual void derived_substitute (Context *f, Context *t);

  virtual void derived_mark () const;
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual void construct_children ();
  virtual bool ok () const;
  virtual void process (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;

private:
  Music_iterator *child_iter_;
  Moment finish_mom_;
};

#endif /* PERCENT_REPEAT_ITERATOR_HH */
