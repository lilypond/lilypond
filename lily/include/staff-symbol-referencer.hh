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

#ifndef STAFF_SYMBOL_REFERENCER_HH
#define STAFF_SYMBOL_REFERENCER_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

/**
   A notation object that needs access to variables of the staff (no
   lines, leading).
*/
class Staff_symbol_referencer
{
public:
  static bool ugly_hack (Grob *);
  static void set_position (Grob *, Real);
  static void pure_set_position (Grob *, Real);
  DECLARE_SCHEME_CALLBACK (callback, (SCM element));

  /**
     Leading are the lead strips between the sticks (lines) of
     typeface. ie. leading is vertical space.
  */
  static Real line_thickness (Grob *);
  static Real staff_space (Grob *);
  static Grob *get_staff_symbol (Grob *);
  static bool on_line (Grob *, int);
  static bool on_staff_line (Grob *, int);
  static Real get_position (Grob *);
  static Real pure_get_position (Grob *);

  /**
     Interval of staff lines.
  */
  static Interval staff_span (Grob *);

  /**
     Half of the height, in staff space, i.e. 2.0 for a normal staff.
  */
  static Real staff_radius (Grob *);

  static int get_rounded_position (Grob *);
  static int pure_get_rounded_position (Grob *);
  static Interval extent_in_staff (Grob *);

private:
  static void internal_set_position (Grob *, Real, bool);
  static Real internal_get_position (Grob *, bool);
};

int compare_position (Grob *const &, Grob *const &);
bool position_less (Grob *const &, Grob *const &);
bool pure_position_less (Grob *const &, Grob *const &);
#endif /* STAFF_SYMBOL_REFERENCER_HH */
