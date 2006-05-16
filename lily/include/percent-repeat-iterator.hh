/*
  percent-repeat-iterator.hh -- declare Percent_repeat_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2001--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PERCENT_REPEAT_ITERATOR_HH
#define PERCENT_REPEAT_ITERATOR_HH

#include "sequential-iterator.hh"

class Percent_repeat_iterator : public Sequential_iterator
{
public:
  DECLARE_CLASSNAME(Percent_repeat_iterator);
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Percent_repeat_iterator ();
protected:
  virtual SCM get_music_list () const;
  virtual void derived_mark () const;
  virtual void construct_children ();
private:
  SCM child_list_;
};

#endif /* PERCENT_REPEAT_ITERATOR_HH */
