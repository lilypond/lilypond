/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "music-wrapper.hh"

class Grace_music
{
public:
  DECLARE_SCHEME_CALLBACK (start_callback, (SCM));
};

MAKE_SCHEME_CALLBACK (Grace_music, start_callback,
                      "ly:grace-music::start-callback", 1);
SCM
Grace_music::start_callback (SCM m)
{
  Moment *l = unsmob<Moment> (Music_wrapper::length_callback (m));
  Moment gl;
  gl.grace_part_ = -(l->main_part_ + l->grace_part_);
  return gl.smobbed_copy ();
}
