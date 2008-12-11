/*
  repeated-music.hh -- declare Repeated_music

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef REPEATED_MUSIC_HH
#define REPEATED_MUSIC_HH

#include "music.hh"

/**
   Musical repeats.

   A repeat, when played has the form of BODY A BODY B BODY C.

   In this examples, the list {A B C} is put in ALTERNATIVES_P_.  BODY
   is put in REPEAT_BODY_P_.  Either the body or the alternative may
   be omitted.

   There are three modes of representing this  music:

   BODY A
   B
   C

   is called "folded". Mostly used for lyrics.


   BODY A B C

   is called volta.  This is common notation

   BODY A BODY B BODY C

   is called unfolded.  Useful for MIDI.

   If the number of repeats is smaller than the number of alternatives, then
   the excess alternatives are ignored for all timing purposes.

   If the number of repeats is bigger than the number of alternatives, then
   the first alternative is assumed to be repeated.
*/
class Repeated_music
{
public:
  static Music *body (Music *);
  static SCM alternatives (Music *);

  /* How often do we repeat? */
  static int repeat_count (Music *);
  DECLARE_SCHEME_CALLBACK (relative_callback, (SCM, SCM));

  static Moment body_get_length (Music *);
  static Moment alternatives_get_length (Music *, bool fold);
  static Moment alternatives_volta_get_length (Music *);

  DECLARE_SCHEME_CALLBACK (unfolded_music_length, (SCM));
  DECLARE_SCHEME_CALLBACK (volta_music_length, (SCM));
  DECLARE_SCHEME_CALLBACK (folded_music_length, (SCM));
  DECLARE_SCHEME_CALLBACK (minimum_start, (SCM));
  DECLARE_SCHEME_CALLBACK (first_start, (SCM));
};

#endif /* REPEATED_MUSIC_HH */
