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

#include "stencil.hh"
#include "protected-scm.hh"

static Protected_scm heads (SCM_EOL);

void
register_stencil_head (SCM symbol)
{
  scm_set_object_property_x (symbol, ly_symbol2scm ("stencil-head?"),
                             SCM_BOOL_T);
  heads = scm_cons (symbol, heads);
}

bool
is_stencil_head (SCM symbol)
{
  return from_scm<bool> (
    scm_object_property (symbol, ly_symbol2scm ("stencil-head?")));
}

SCM
all_stencil_heads ()
{
  return heads;
}
