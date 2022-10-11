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

#ifndef TIE_HH
#define TIE_HH

#include "lily-proto.hh"
#include "skyline.hh"
#include "grob-interface.hh"

class Tie
{
public:
  static void set_head (Spanner *, Direction, Grob *head);
  static Item *head (Spanner *, Direction);
  static int get_column_rank (Spanner *, Direction);
  static int get_position (Spanner *);
  static Direction get_default_dir (Spanner *);
  static SCM get_control_points (Grob *, Grob *, Tie_configuration const &,
                                 Tie_details const &);
  static SCM get_default_control_points (Spanner *);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_control_points, (SCM));
  static bool less (Grob *s1, Grob *s2);
};

#endif // TIE_HH
