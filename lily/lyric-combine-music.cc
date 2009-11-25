/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "music.hh"

struct Lyric_combine_music
{
public:
  DECLARE_SCHEME_CALLBACK (length_callback, (SCM));
};

MAKE_SCHEME_CALLBACK (Lyric_combine_music, length_callback, 1);
SCM
Lyric_combine_music::length_callback (SCM m)
{
  Music *me = unsmob_music (m);
  Music *melody = unsmob_music (scm_car (me->get_property ("elements")));
  return melody->get_length ().smobbed_copy ();
}
