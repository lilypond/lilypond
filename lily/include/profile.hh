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

#ifndef PROFILE_HH
#define PROFILE_HH

#include "lily-guile.hh"

class Protected_scm;

void note_property_access (Protected_scm *table, SCM sym);
extern Protected_scm context_property_lookup_table;
extern Protected_scm grob_property_lookup_table;
extern Protected_scm prob_property_lookup_table;
extern bool profile_property_accesses;

#endif /* PROFILE_HH */
