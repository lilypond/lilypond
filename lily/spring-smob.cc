/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "spring.hh"
#include "warn.hh"
#include "ly-smobs.icc"

IMPLEMENT_SIMPLE_SMOBS (Spring);

SCM
Spring::mark_smob (SCM)
{
  return SCM_UNSPECIFIED;
}

int
Spring::print_smob (SCM, SCM p, scm_print_state *)
{
  scm_puts ("#<Spring smob>", p);
  return 1;
}

SCM
Spring::equal_p (SCM a, SCM b)
{
  return a == b? SCM_BOOL_T : SCM_BOOL_F;
}

