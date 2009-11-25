/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "identifier-smob.hh"

scm_t_bits package_tag;

static int
print_box (SCM b, SCM port, scm_print_state *)
{
  SCM value = SCM_CELL_OBJECT_1 (b);

  scm_puts ("#<packaged object ", port);
  scm_write (value, port);
  scm_puts (">", port);

  /* Non-zero means success.  */
  return 1;
}

/* This defines the primitve `make-box', which returns a new smob of
   type `box', initialized to `#f'.  */
LY_DEFINE (ly_export, "ly:export",
	   1, 0, 0, (SCM arg),
	   "Export a Scheme object to the parser"
	   " so it is treated as an identifier.")
{
  SCM_RETURN_NEWSMOB (package_tag, arg);
}

SCM
unpack_identifier (SCM box)
{
  if (SCM_IMP (box) || SCM_CELL_TYPE (box) != package_tag)
    return SCM_UNDEFINED;

  return SCM_CELL_OBJECT_1 (box);
}

static void
init_box_type (void)
{
  package_tag = scm_make_smob_type ("box", 0);
  scm_set_smob_mark (package_tag, scm_markcdr);
  scm_set_smob_print (package_tag, print_box);
}

ADD_SCM_INIT_FUNC (package, init_box_type);
