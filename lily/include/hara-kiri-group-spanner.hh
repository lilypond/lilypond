/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
#define HARA_KIRI_VERTICAL_GROUP_SPANNER_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

class Hara_kiri_group_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (force_hara_kiri_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (y_extent, (SCM smob));
  DECLARE_SCHEME_CALLBACK (calc_skylines, (SCM smob));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM smob, SCM start, SCM end));
  DECLARE_SCHEME_CALLBACK (force_hara_kiri_in_y_parent_callback, (SCM));
  static bool request_suicide (Grob *me, vsize start, vsize end);
  static bool request_suicide_alone (Grob *me, vsize start, vsize end);
  static void consider_suicide (Grob *me);
  static void add_interesting_item (Grob *me, Grob *n);
};

#endif // HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
