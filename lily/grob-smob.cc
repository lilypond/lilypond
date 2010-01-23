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

#include "grob.hh"

#include "paper-score.hh"
#include "warn.hh"

#include "ly-smobs.icc"

IMPLEMENT_SMOBS (Grob);
IMPLEMENT_DEFAULT_EQUAL_P (Grob);
IMPLEMENT_TYPE_P (Grob, "ly:grob?");

SCM
Grob::mark_smob (SCM ses)
{
  ASSERT_LIVE_IS_ALLOWED ();
  
  Grob *s = (Grob *) SCM_CELL_WORD_1 (ses);
  scm_gc_mark (s->immutable_property_alist_);

  /* Do not mark the parents.  The pointers in the mutable
     property list form two tree like structures (one for X
     relations, one for Y relations).  Marking these can be done
     in limited stack space.  If we add the parents, we will jump
     between X and Y in an erratic manner, leading to much more
     recursion depth (and core dumps if we link to pthreads).  */

  if (s->original ())
    scm_gc_mark (s->original ()->self_scm ());

  s->derived_mark ();
  scm_gc_mark (s->object_alist_);
  scm_gc_mark (s->interfaces_);

  return s->mutable_property_alist_;
}

int
Grob::print_smob (SCM s, SCM port, scm_print_state *)
{
  Grob *sc = (Grob *) SCM_CELL_WORD_1 (s);

  scm_puts ("#<Grob ", port);
  scm_puts ((char *) sc->name ().c_str (), port);

  /* Do not print properties, that is too much hassle.  */
  scm_puts (" >", port);
  return 1;
}

void
Grob::derived_mark () const
{
}
