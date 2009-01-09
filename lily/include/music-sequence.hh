/*
  music-sequence.hh -- declare Music_sequence

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MUSIC_SEQUENCE_HH
#define MUSIC_SEQUENCE_HH

#include "pitch.hh"
#include "moment.hh"
#include "lily-guile.hh"

struct Music_sequence
{
public:
  DECLARE_SCHEME_CALLBACK (cumulative_length_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (maximum_length_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (minimum_start_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (first_start_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (simultaneous_relative_callback, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (event_chord_relative_callback, (SCM, SCM));

  Pitch do_relative_octave (Pitch p, bool b);

  static Moment cumulative_length (SCM);
  static Moment maximum_length (SCM);
  static Moment first_start (SCM list);
  static Moment minimum_start (SCM list);
};

SCM ly_transpose_key_alist (SCM l, SCM pit);
Pitch music_list_to_relative (SCM l, Pitch p, bool ret_first);
void transpose_music_list (SCM, Pitch);
void compress_music_list (SCM, Moment);

#endif
