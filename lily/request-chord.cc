#include "pitch.hh" 
#include "event-chord-iterator.hh"
#include "music-list.hh"
#include "event.hh"

Event_chord::Event_chord ()
{
}

Moment
Event_chord::start_mom () const
{
  return Music::start_mom ();
}

ADD_MUSIC (Event_chord);
