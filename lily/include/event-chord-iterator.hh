/*
  event-chord-iterator.hh -- declare Event_chord_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef EVENT_CHORD_ITERATOR_HH
#define EVENT_CHORD_ITERATOR_HH

#include "simple-music-iterator.hh"

/**
   Walk through a Event_chord
*/
class Event_chord_iterator : public Simple_music_iterator
{
  /**
     Find a bottom notation context to deliver events to.
  */
  DECLARE_CLASSNAME(Event_chord_iterator);

public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Event_chord_iterator ();
  Event_chord_iterator (Event_chord_iterator const &);

protected:
  virtual void process (Moment);
  virtual void construct_children ();
};

#endif // EVENT_CHORD_ITERATOR_HH
