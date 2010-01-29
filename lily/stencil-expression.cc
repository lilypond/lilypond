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

#include "stencil.hh"

static SCM heads;

void register_stencil_head (SCM symbol)
{
  if (!heads)
    heads = scm_permanent_object (scm_cons (SCM_EOL, SCM_EOL));
  
  scm_set_object_property_x (symbol, ly_symbol2scm ("stencil-head?"), SCM_BOOL_T);
  scm_set_cdr_x (heads,  scm_cons (symbol, scm_cdr (heads)));
}

bool
is_stencil_head (SCM symbol)
{
  return scm_object_property (symbol, ly_symbol2scm ("stencil-head?"))
    == SCM_BOOL_T;
}

SCM
all_stencil_heads ()
{
  return scm_cdr (heads);
}

