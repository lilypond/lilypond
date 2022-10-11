/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef STENCIL_INTERPRET_HH
#define STENCIL_INTERPRET_HH

#include "lily-guile.hh"

class Stencil_sink
{
public:
  virtual SCM output (SCM expr) = 0;
};

void interpret_stencil_expression (SCM expr, Stencil_sink *sink, Offset o);

#endif
