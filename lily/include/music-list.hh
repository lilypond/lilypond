/*
  music-list.hh -- declare Simultaneous_music, Sequential_music, Event_chord

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef MUSIC_LIST_HH
#define MUSIC_LIST_HH

#include "music-sequence.hh"

/**
  Simultaneous_music is a list of music-elements which happen simultaneously
 */
class Simultaneous_music : public Music_sequence
{
public:
  Simultaneous_music (SCM);
  VIRTUAL_COPY_CONSTRUCTOR (Music, Simultaneous_music);
  virtual Pitch to_relative_octave (Pitch);
};

/*
  A chord.
 */
class Event_chord : public Simultaneous_music
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Music, Event_chord);
  virtual Pitch to_relative_octave (Pitch);
  Event_chord (SCM);
};

/**
  Sequential_music is a list of music-elements which are placed behind each other.
 */
class Sequential_music : public Music_sequence
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Music, Sequential_music);
  Sequential_music (SCM);
};

#endif /* MUSIC_LIST_HH */
