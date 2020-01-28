/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef VOLTA_SPANNER_HH
#define VOLTA_SPANNER_HH

#include "lily-proto.hh"
#include "spanner.hh"

class Volta_bracket_interface
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static void modify_edge_height (Spanner *);
  static void add_bar (Grob *me, Item *bar);
};

#endif // VOLTA_SPANNER_HH
