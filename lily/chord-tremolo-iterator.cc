/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>
                 Erik Sandberg <mandolaerik@gmail.com>

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

#include "chord-tremolo-iterator.hh"

#include "repeated-music.hh"
#include "lily-imports.hh"

Chord_tremolo_iterator::Chord_tremolo_iterator ()
{
}

SCM
Chord_tremolo_iterator::get_music_list () const
{
  return Lily::tremolo_get_music_list (get_music ()->self_scm ());
}

IMPLEMENT_CTOR_CALLBACK (Chord_tremolo_iterator);
