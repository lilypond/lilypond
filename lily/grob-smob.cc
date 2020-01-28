/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

const char *const Grob::type_p_name_ = "ly:grob?";

SCM
Grob::mark_smob () const
{
  ASSERT_LIVE_IS_ALLOWED (self_scm ());

  scm_gc_mark (immutable_property_alist_);

  /* Do not mark the parents.  The pointers in the mutable
     property list form two tree like structures (one for X
     relations, one for Y relations).  Marking these can be done
     in limited stack space.  If we add the parents, we will jump
     between X and Y in an erratic manner, leading to much more
     recursion depth (and core dumps if we link to pthreads).  */

  if (original ())
    scm_gc_mark (original ()->self_scm ());

  derived_mark ();
  scm_gc_mark (object_alist_);
  scm_gc_mark (interfaces_);

  return mutable_property_alist_;
}

int
Grob::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<Grob ", port);
  scm_puts ((char *)name ().c_str (), port);

  /* Do not print properties, that is too much hassle.  */
  scm_puts (" >", port);
  return 1;
}

void
Grob::derived_mark () const
{
}
