/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef RELOCATE_HH
#define RELOCATE_HH

#include "std-string.hh"

void read_relocation_dir (string dirname);
void read_relocation_file (string filename);
string expand_environment_variables (string orig);

int sane_putenv (char const *key, string value, bool overwrite);
void setup_paths (char const *argv0);
extern bool relocate_binary;

#endif /* RELOCATE_HH */
