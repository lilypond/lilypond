/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2010 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef LILYPOND_VERSION_HH
#define LILYPOND_VERSION_HH

#include "std-string.hh"

struct Lilypond_version
{
  Lilypond_version (int major, int minor, int patch);
  Lilypond_version (string str);

  string to_string () const;
  operator int () const;

  int major_;
  int minor_;
  int patch_;
  string extra_patch_string_;
};

extern Lilypond_version oldest_version;

#endif // LILYPOND_VERSION_HH
