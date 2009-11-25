/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef HORIZONTAL_BRACKET_HH
#define HORIZONTAL_BRACKET_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "grob-interface.hh"

struct Horizontal_bracket
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static Stencil make_bracket (Grob *, Real, Axis, Direction);
  static Stencil make_enclosing_bracket (Grob *me, Grob *refpoint,
					 vector<Grob*> grobs,
					 Axis a, Direction dir);
  DECLARE_GROB_INTERFACE();
};

#endif /* HORIZONTAL_BRACKET_HH */
