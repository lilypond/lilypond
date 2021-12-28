/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef MUSIC_SEQUENCE_HH
#define MUSIC_SEQUENCE_HH

#include "pitch.hh"
#include "moment.hh"
#include "lily-guile.hh"

class Music_sequence
{
public:
  DECLARE_SCHEME_CALLBACK (cumulative_length_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (maximum_length_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (event_chord_length_callback, (SCM));
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

#endif
