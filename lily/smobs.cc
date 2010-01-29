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

#include "smobs.hh"

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
protect_smob (SCM smob, SCM *prot_cons)
{
#if 0
  SCM s = scm_cdr (smob_protection_list);
  while (scm_is_pair (s) && scm_car (s) == SCM_BOOL_F)
    {
      s = scm_cdr (s);
    }
  SCM prot = scm_cons (smob, s);
  scm_set_cdr_x (smob_protection_list,
		 prot);
  *prot_cons = prot;
#else
  (void) prot_cons; 
  scm_gc_protect_object (smob);
#endif
}

void
unprotect_smob (SCM smob, SCM *prot_cons)
{
#if 1
  (void) prot_cons; 
  scm_gc_unprotect_object (smob);
#else
  SCM next = scm_cdr (*prot_cons);

  if (next == SCM_EOL)
    scm_set_car_x (*prot_cons, SCM_BOOL_F);
  else
    {
      scm_set_car_x (*prot_cons, SCM_BOOL_F);
      while (scm_is_pair (next)
	     && scm_car (next) == SCM_BOOL_F)

	next = scm_cdr (next);

      scm_set_cdr_x (*prot_cons, next);
    }

  *prot_cons = SCM_EOL;
#endif
}
