/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "warn.hh"
#include "program-option.hh"

class Relative_octave_music
{
public:
  DECLARE_SCHEME_CALLBACK (relative_callback, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (no_relative_callback, (SCM, SCM));
};

MAKE_SCHEME_CALLBACK (Relative_octave_music, no_relative_callback,
                      "ly:relative-octave-music::no-relative-callback", 2)
SCM
Relative_octave_music::no_relative_callback (SCM /* music */, SCM pitch)
{
  return pitch;
}

MAKE_SCHEME_CALLBACK (Relative_octave_music, relative_callback,
                      "ly:relative-octave-music::relative-callback", 2)
SCM
Relative_octave_music::relative_callback (SCM /* music */, SCM pitch)
{
  return pitch;
}
