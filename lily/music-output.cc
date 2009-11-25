/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "music-output.hh"

#include "ly-smobs.icc"

Music_output::Music_output ()
{
  smobify_self ();
}

void
Music_output::process ()
{
}

Music_output::~Music_output ()
{
}

void
Music_output::derived_mark () const
{
}

IMPLEMENT_SMOBS (Music_output);
IMPLEMENT_DEFAULT_EQUAL_P (Music_output);
IMPLEMENT_TYPE_P (Music_output, "ly:music-output?");

SCM
Music_output::mark_smob (SCM s)
{
  Music_output *sc = (Music_output *) SCM_CELL_WORD_1 (s);

  sc->derived_mark ();
  return SCM_EOL;
}

int
Music_output::print_smob (SCM s, SCM p, scm_print_state*)
{
  Music_output *sc = (Music_output *) SCM_CELL_WORD_1 (s);
  scm_puts ("#<", p);
  scm_puts (sc->class_name (), p);
  scm_puts (">", p);

  return 1;
}
