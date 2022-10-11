/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SLUR_HH
#define SLUR_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

#include <vector>

class Slur
{
public:
  static void add_column (Spanner *me, Grob *col);
  static void add_extra_encompass (Spanner *me, Grob *col);
  static void replace_breakable_encompass_objects (Grob *me);
  static void auxiliary_acknowledge_extra_object (Grob *,
                                                  std::vector<Spanner *> &,
                                                  std::vector<Spanner *> &);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_control_points, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (vertical_skylines, (SCM));
  DECLARE_SCHEME_CALLBACK (outside_slur_callback, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (pure_outside_slur_callback, (SCM, SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (outside_slur_cross_staff, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  static Bezier get_curve (Grob *me);
};

#endif /* SLUR_HH */
