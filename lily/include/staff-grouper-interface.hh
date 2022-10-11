/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2009--2022 Joe Neeman <joeneeman@gmail.com>

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

#ifndef STAFF_GROUPER_INTERFACE_HH
#define STAFF_GROUPER_INTERFACE_HH

#include "grob.hh"

class Staff_grouper_interface
{
public:
  static Grob *get_extremal_staff (Grob *, Grob *refpoint, Direction dir,
                                   Interval const &iv);
  static bool maybe_pure_within_group (Grob *, Grob *child, bool, int, int);
};

#endif /* STAFF_GROUPER_INTERFACE_HH */
