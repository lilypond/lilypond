/*
  event.hh -- declare Event baseclasses.

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  virtual void compress (Moment);
  virtual Moment get_length () const;
  virtual Pitch to_relative_octave (Pitch);
};


/**
    Handle key changes.
*/
class Key_change_ev  : public Event
{
public:
  Key_change_ev (SCM);
  SCM pitch_alist ();
  
protected:
  VIRTUAL_COPY_CONSTRUCTOR (Music, Key_change_ev);
  void transpose (Pitch  d);
};

SCM ly_transpose_key_alist (SCM,SCM);



#endif
