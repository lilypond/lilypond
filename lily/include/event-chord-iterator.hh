/*
  event-iter.hh -- declare Event_chord_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef EVENT_ITER_HH
#define EVENT_ITER_HH

#include "simple-music-iterator.hh"

/**
   Walk through a Event_chord
 */
class Event_chord_iterator : public Simple_music_iterator
{
  Event_chord * get_elt () const;
  /**
     Find a bottom notation context to deliver events to.
   */
  virtual Context * get_req_translator ();


  /*
    Since Event_chord_iterator has no list-cursor internally, we
    must use a status variable to adminstrate where we are */
  
  enum { NONE_DONE, START_DONE, END_DONE }  status_;
public:
  DECLARE_SCHEME_CALLBACK(constructor, ());
  Event_chord_iterator ();
  Event_chord_iterator (Event_chord_iterator const&);

protected:
  virtual void process (Moment);
  virtual void construct_children ();
};


#endif // EVENT_ITER_HH
