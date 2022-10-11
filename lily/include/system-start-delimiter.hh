/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SYSTEM_START_DELIMITER_HH
#define SYSTEM_START_DELIMITER_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

/*
  Braces/brackets across staves.
*/
class System_start_delimiter
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));

  static void try_collapse (Grob *);
  static Stencil staff_bracket (Grob *, Real);
  static Stencil old_staff_bracket (Grob *, Real);
  static Stencil staff_brace (Grob *, Real);
  static Stencil simple_bar (Grob *, Real);
  static Stencil line_bracket (Grob *, Real);
};

#endif /* SYSTEM_START_DELIMITER_HH */
