/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2014 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

const char Music_function::type_p_name_[] = "ly:music-function?";

/* Print a textual represenation of the smob to a given port.  */
int
Music_function::print_smob (SCM port, scm_print_state *)
{
  scm_puts ("#<Music function ", port);
  scm_write (get_function (), port);
  scm_puts (">", port);

  /* Non-zero means success.  */
  return 1;
}

SCM
Music_function::mark_smob ()
{
  ASSERT_LIVE_IS_ALLOWED (self_scm ());
  return Smob2<Music_function>::mark_smob ();
}
