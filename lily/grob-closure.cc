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

#include "grob.hh"
#include "unpure-pure-container.hh"
#include "lily-imports.hh"

SCM
axis_offset_symbol (Axis a)
{
  return a == X_AXIS ? ly_symbol2scm ("X-offset") : ly_symbol2scm ("Y-offset");
}

SCM
axis_parent_positioning (Axis a)
{
  return (a == X_AXIS) ? Grob::x_parent_positioning_proc
                       : Grob::y_parent_positioning_proc;
}

/*
  Replace

  (orig-proc GROB)

  by

  (+ (PROC GROB) (orig-proc GROB))
*/
void
add_offset_callback (Grob *g, SCM proc, Axis a)
{
  SCM sym = axis_offset_symbol (a);
  SCM data = get_property_data (g, sym);
  set_property (g, sym, Lily::grob_offset_function (proc, data));
}

/*
  replace

  (orig-proc GROB)

  by

  (PROC GROB (orig-proc GROB))
*/
void
chain_callback (Grob *g, SCM proc, SCM sym)
{
  SCM data = get_property_data (g, sym);
  set_property (g, sym, Lily::grob_compose_function (proc, data));
}

void
chain_offset_callback (Grob *g, SCM proc, Axis a)
{
  chain_callback (g, proc, axis_offset_symbol (a));
}
