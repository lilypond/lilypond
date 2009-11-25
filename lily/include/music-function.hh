/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef MUSIC_FUNCTION_HH
#define MUSIC_FUNCTION_HH

#include "lily-guile.hh"

SCM ly_make_music_function (SCM, SCM);
SCM make_music_function (SCM, SCM);

SCM get_music_function_transform (SCM);
bool is_music_function (SCM);

#endif /* MUSIC_FUNCTION_HH */

