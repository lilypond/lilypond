/*
  event-iter.hh -- declare Event_chord_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2006--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
  	   Erik Sandberg <mandolaerik@gmail.com>
*/

#ifndef EVENT_ITERATOR_HH
#define EVENT_ITERATOR_HH

#include "simple-music-iterator.hh"

class Event_iterator : public Simple_music_iterator
{
  DECLARE_CLASSNAME(Event_iterator);

public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Event_iterator ();
  Event_iterator (Event_iterator const &);
  virtual void construct_children ();

protected:
  virtual void process (Moment);
};

#endif // EVENT_ITERATOR_HH
