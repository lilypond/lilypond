/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "ly-smobs.icc"

class Musicfunction
{
  DECLARE_SIMPLE_SMOBS (Musicfunction);
  SCM signature_;
  SCM function_;
public:
  Musicfunction (SCM signature, SCM function):
    signature_ (signature), function_ (function) { }
  SCM get_function () { return function_; }
  SCM get_signature () { return signature_; }
};

IMPLEMENT_SIMPLE_SMOBS (Musicfunction);
IMPLEMENT_DEFAULT_EQUAL_P (Musicfunction);

/* Print a textual represenation of the smob to a given port.  */
int
Musicfunction::print_smob (SCM b, SCM port, scm_print_state *)
{
  scm_puts ("#<Music function ", port);
  scm_write (Musicfunction::unsmob (b)->get_function (), port);
  scm_puts (">", port);

  /* Non-zero means success.  */
  return 1;
}

bool
is_music_function (SCM music_function)
{
  return Musicfunction::unsmob (music_function);
}

SCM
get_music_function_transform (SCM music_function)
{
  if (!is_music_function (music_function))
    return SCM_UNDEFINED;

  return Musicfunction::unsmob (music_function)->get_function ();
}

SCM
make_music_function (SCM signature, SCM func)
{
  return Musicfunction (signature, func).smobbed_copy ();
}

SCM
get_music_function_signature (SCM music_function)
{
  if (!is_music_function (music_function))
    return SCM_UNDEFINED;

  return Musicfunction::unsmob (music_function)->get_signature ();
}

SCM
Musicfunction::mark_smob (SCM s)
{
  Musicfunction *p = Musicfunction::unsmob (s);
  scm_gc_mark (p->signature_);
  ASSERT_LIVE_IS_ALLOWED (s);
  return p->function_;
}
