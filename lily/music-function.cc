/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "music-function.hh"

#include "music.hh"

static scm_t_bits music_function_tag;

/* Print a textual represenation of the smob to a given port.  */
static int
print_music_function (SCM b, SCM port, scm_print_state *)
{
  SCM value = SCM_CELL_OBJECT_1 (b);

  scm_puts ("#<Music function ", port);
  scm_write (value, port);
  scm_puts (">", port);

  /* Non-zero means success.  */
  return 1;
}

bool
is_music_function (SCM music_function)
{
  return (SCM_NIMP (music_function) && SCM_CELL_TYPE (music_function) == music_function_tag);
}

SCM
get_music_function_transform (SCM music_function)
{
  if (!is_music_function (music_function))
    return SCM_UNDEFINED;

  return SCM_CELL_OBJECT_1 (music_function);
}

static void
init_music_function (void)
{
  music_function_tag = scm_make_smob_type ("music-function", 0);
  scm_set_smob_mark (music_function_tag, scm_markcdr);
  scm_set_smob_print (music_function_tag, print_music_function);
}

SCM
make_music_function (SCM signature, SCM func)
{
  scm_set_object_property_x (func, ly_symbol2scm ("music-function-signature"),
			     signature);

  SCM_RETURN_NEWSMOB (music_function_tag, func);
}

ADD_SCM_INIT_FUNC (music_function_tag, init_music_function);

