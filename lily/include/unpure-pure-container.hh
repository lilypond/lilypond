/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2011 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef UNPURE_PURE_CONTAINER_HH
#define UNPURE_PURE_CONTAINER_HH

#include "lily-guile.hh"

bool is_unpure_pure_container (SCM s);
SCM unpure_pure_container_unpure_part (SCM smob);
SCM unpure_pure_container_pure_part (SCM smob);
SCM ly_make_unpure_pure_container (SCM, SCM);

#endif /* UNPURE_PURE_CONTAINER_HH */
