/*
  event.hh -- declare Event baseclasses.

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef EVENT_HH
#define EVENT_HH

#include "input.hh"
#include "music.hh"
#include "duration.hh"
#include "pitch.hh"

/* An atom of musical information.  This is an abstract class for any
   piece of music that does not contain other Music.  */
class Event : public Music
{
public:
  Event (SCM);
  VIRTUAL_COPY_CONSTRUCTOR (Music, Event);
  DECLARE_SCHEME_CALLBACK (length_callback, (SCM));
};


#endif
