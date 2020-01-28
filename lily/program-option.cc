/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2020 Han-Wen Nienhuys <hanwen@lilypond.org>


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

#include "program-option.hh"

using std::string;

string
get_output_backend_name ()
{
  return ly_symbol2string (ly_get_option (ly_symbol2scm ("backend")));
}

bool
get_program_option (const char *s)
{
  SCM sym = ly_symbol2scm (s);

  return to_boolean (ly_get_option (sym));
}
