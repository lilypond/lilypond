#include "pitch.hh" 
#include "request-chord-iterator.hh"
#include "music-list.hh"
#include "request.hh"

Request_chord::Request_chord ()
{
}

Moment
Request_chord::start_mom () const
{
  return Music::start_mom ();
}

ADD_MUSIC (Request_chord);
