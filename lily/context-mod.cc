/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2011 Reinhold Kainhofer <reinhold@kainhofer.com>

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

#include "context-mod.hh"

Context_mod::Context_mod ()
{
  mods_ = SCM_EOL;
}

Context_mod::Context_mod (Context_mod const &s)
{
  mods_ = s.mods_;
}

Context_mod::Context_mod (SCM mod_list)
{
  mods_ = scm_reverse (mod_list);
}

#include "ly-smobs.icc"
IMPLEMENT_SIMPLE_SMOBS (Context_mod);
IMPLEMENT_DEFAULT_EQUAL_P (Context_mod);
IMPLEMENT_TYPE_P (Context_mod, "ly:context-mod?");

int
Context_mod::print_smob (SCM smob, SCM port, scm_print_state *)
{
  Context_mod *me = (Context_mod *) SCM_CELL_WORD_1 (smob);

  scm_puts ("#<Context_mod ", port);
  scm_display (me->mods_, port);
  scm_puts (">", port);
  return 1;
}

SCM
Context_mod::mark_smob (SCM smob)
{
  ASSERT_LIVE_IS_ALLOWED ();

  Context_mod *me = (Context_mod *) SCM_CELL_WORD_1 (smob);

  scm_gc_mark (me->mods_);
  return me->mods_;
}

void
Context_mod::add_context_mod (SCM mod)
{
  mods_ = scm_cons (mod, mods_);
}

void
Context_mod::add_context_mods (SCM mods)
{
  for (SCM m = mods; scm_is_pair (m); m = scm_cdr (m))
    add_context_mod (scm_car (m));
}

SCM
Context_mod::get_mods () const
{
  return scm_reverse (mods_);
}

