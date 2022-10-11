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

#ifndef DIRECTIONAL_ELEMENT_HH
#define DIRECTIONAL_ELEMENT_HH

#include "lily-proto.hh"
#include "direction.hh"

// what is the advantage not having these three as STATICs of GROB -- jcn
void set_grob_direction (Grob *, Direction);
Direction get_grob_direction (Grob *);
Direction get_strict_grob_direction (Grob *);

#endif /* DIRECTIONAL_ELEMENT_HH */
