/*
  music-list.hh -- declare Music_sequence,
  Simultaneous_music and Sequential_music

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  VIRTUAL_COPY_CONS (Music);
  virtual Pitch to_relative_octave (Pitch);
  virtual Moment get_length () const;
  virtual Moment start_mom () const;
  Simultaneous_music ();
};

/**
  The event is a collection of Events. A note that you enter in lilypond is 
  one Event_chord, one syllable of lyrics is one Event_chord
 */
class Event_chord : public Simultaneous_music
{
public:
  VIRTUAL_COPY_CONS (Music);
  virtual Moment start_mom () const;

  Event_chord ();
};

/**
  Sequential_music is a list of music-elements which are placed behind each other.
 */
class Sequential_music : public Music_sequence
{
public:
  VIRTUAL_COPY_CONS (Music);
  virtual Moment get_length () const;
  virtual Moment start_mom () const;
  
  Sequential_music ();
};

#endif // Music_sequence_HH
