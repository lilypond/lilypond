/*
  music-list.hh -- declare Music_sequence,
  Simultaneous_music and Sequential_music

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Music_sequence_HH
#define Music_sequence_HH

#include "music-sequence.hh"

/**
  Simultaneous_music is a list of music-elements which happen simultaneously
 */
class Simultaneous_music : public Music_sequence
{
public:
  VIRTUAL_COPY_CONS(Music);
  Simultaneous_music(SCM);
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  virtual Moment length_mom () const;
};

/**
  The request is a collection of Requests. A note that you enter in mudela is 
  one Request_chord, one syllable of lyrics is one Request_chord
 */
class Request_chord : public Simultaneous_music
{
public:
  VIRTUAL_COPY_CONS(Music);
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  Request_chord(SCM list);
};

/**
  Sequential_music is a list of music-elements which are placed behind each other.
 */
class Sequential_music : public Music_sequence
{
public:
  VIRTUAL_COPY_CONS(Music);
  Sequential_music(SCM);
  virtual Moment length_mom () const;
};

#endif // Music_sequence_HH
