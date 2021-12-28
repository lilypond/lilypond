/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#ifndef CALCULATED_SEQUENTIAL_MUSIC_HH
#define CALCULATED_SEQUENTIAL_MUSIC_HH

#include "lily-guile.hh"

class Music;

class Calculated_sequential_music
{
public:
  static SCM calc_elements (Music *);

  DECLARE_SCHEME_CALLBACK (length, (SCM));
  DECLARE_SCHEME_CALLBACK (start, (SCM));
};

#endif /* CALCULATED_SEQUENTIAL_MUSIC_HH */
