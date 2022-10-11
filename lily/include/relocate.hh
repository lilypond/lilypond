/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

void read_relocation_dir (const std::string &dirname);
void read_relocation_file (const std::string &filename);

int sane_putenv (char const *key, const std::string &value, bool overwrite,
                 bool indent = false);
void setup_paths (char const *argv0);

#endif /* RELOCATE_HH */
