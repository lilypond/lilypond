/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef DOT_COLUMN_HH
#define DOT_COLUMN_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

class Grob;

/**
   Group dots.  This is needed because, the dots have to be aligned per voice
*/
class Dot_column // interface
{
public:
  static int compare (Grob *const &, Grob *const &);
  static void add_head (Grob *dotcol, Grob *rh);

  DECLARE_SCHEME_CALLBACK (side_position, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
};
#endif // DOT_COLUMN_HH
