/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
