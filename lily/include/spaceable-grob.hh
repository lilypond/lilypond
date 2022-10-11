/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SPACEABLE_GROB_HH
#define SPACEABLE_GROB_HH

#include "lily-proto.hh"
#include "grob-interface.hh"
#include "spring.hh"

struct Spaceable_grob
{
  /// set a minimum distance
  static void add_rod (Paper_column *me, Paper_column *to, Real distance);
  static void add_spring (Grob *me, Grob *to, Spring const &sp);
  static Spring get_spring (Paper_column *me, Grob *other);

  static SCM get_minimum_distances (Grob *);
  static SCM get_ideal_distances (Grob *);
};

#endif /* SPACEABLE_GROB_HH */
