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

#ifndef FINGERING_COLUMN_HH
#define FINGERING_COLUMN_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

struct Fingering_column
{
  static void add_fingering (Grob *, Grob *);
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
  static void do_x_positioning (Grob *me);
  static void do_y_positioning (Grob *me);
};

#endif /* FINGERING_COLUMN_HH */
