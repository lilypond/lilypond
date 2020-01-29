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

#include "smobs.hh"
#include "listener.hh"

Listener
Smob_core::get_listener (SCM callback)
{
  return Listener (callback, self_scm ());
}

/*
  The CDR contains the actual protected list.
 */
static SCM smob_protection_list = SCM_EOL;

void
init_smob_protection ()
{
  smob_protection_list = scm_cons (SCM_BOOL_F, SCM_EOL);
  scm_gc_protect_object (smob_protection_list);
}
ADD_SCM_INIT_FUNC (init_smob_protection, init_smob_protection);

LY_DEFINE (ly_smob_protects, "ly:smob-protects",
           0, 0, 0, (),
           "Return LilyPond's internal smob protection list.")
{
  return scm_is_pair (smob_protection_list)
         ? scm_cdr (smob_protection_list)
         : SCM_EOL;
}

void
protect_smob (SCM smob)
{
  scm_gc_protect_object (smob);
}

void
unprotect_smob (SCM smob)
{
  scm_gc_unprotect_object (smob);
}


Scm_init const *Scm_init::list_ = 0;

void
Scm_init::init ()
{
  for (Scm_init const *p = list_; p; p = p->next_)
    p->fun_ ();
}
