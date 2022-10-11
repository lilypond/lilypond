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

#ifndef SCRIPT_INTERFACE_HH
#define SCRIPT_INTERFACE_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

/**
   Articulation marks (and the like) that are attached to notes/stems.
   Needs support from Staff_side for proper operation.  Staff_side
   handles the positioning.
*/
class Script_interface
{
public:
  static Stencil get_stencil (Grob *, Direction d);
  static Direction get_direction (Grob *);

  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));

  // return g1.script-priority < g2.script-priority
  static bool script_priority_less (const Grob *g1, const Grob *g2);
};

void make_script_from_event (Grob *p, Context *tg, SCM type, size_t index);

#endif /* SCRIPT_INTERFACE_HH */
