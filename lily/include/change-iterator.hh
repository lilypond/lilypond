/*
  change-iterator.hh -- declare Change_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef CHANGE_ITERATOR_HH
#define CHANGE_ITERATOR_HH

#include "simple-music-iterator.hh"

class Change_iterator : public Simple_music_iterator
{
public:
  /* constructor is public */
  virtual void process (Moment);
  DECLARE_SCHEME_CALLBACK (constructor, ());
  DECLARE_CLASSNAME(Change_iterator);

private:
  void error (string);
};

#endif
