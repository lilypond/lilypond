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

#ifndef SIDE_POSITION_INTERFACE_HH
#define SIDE_POSITION_INTERFACE_HH

#include "axis.hh"
#include "grob-interface.hh"
#include "lily-proto.hh"

/*
  TODO: move out unrelated callbacks.

  TODO: reduce number of methods.
*/
struct Side_position_interface
{
public:
  DECLARE_SCHEME_CALLBACK (x_aligned_side, (SCM element, SCM current));
  DECLARE_SCHEME_CALLBACK (y_aligned_side, (SCM element, SCM current));
  DECLARE_SCHEME_CALLBACK (pure_y_aligned_side,
                           (SCM element, SCM start, SCM end, SCM current));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM element));
  DECLARE_SCHEME_CALLBACK (move_to_extremal_staff, (SCM));
  DECLARE_SCHEME_CALLBACK (set_axis_x, (SCM, SCM));

  static SCM aligned_side (Grob *me, Axis a, bool pure, int start, int end,
                           Real *current_off_ptr);

  static bool is_on_x_axis (Grob *);
  static bool is_on_y_axis (Grob *);
  static void set_axis (Grob *, Axis);
  static void add_support (Grob *, Grob *);
  static void recursive_add_support (Grob *, Grob *);
  static void add_staff_support (Grob *);
};

#endif /* SIDE_POSITION_INTERFACE_HH */
