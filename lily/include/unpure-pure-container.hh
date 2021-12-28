/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef UNPURE_PURE_CONTAINER_HH
#define UNPURE_PURE_CONTAINER_HH

#include "lily-guile.hh"
#include "small-smobs.hh"

class Unpure_pure_container : public Smob2<Unpure_pure_container>
{
public:
  static const char *const type_p_name_;
  SCM unpure_part () const { return scm1 (); }
  // A container that has the same callback for both 'pure' and 'unpure' lookups
  // and which ignores the 'start' and 'end' columnns.
  // Such a callback will give the same answer for tentative or final layouts.
  bool is_unchanging () const { return SCM_UNBNDP (scm2 ()); }
  SCM pure_part () const;
  static SCM make_smob (SCM a, SCM b = SCM_UNDEFINED)
  {
    if (SCM_UNBNDP (b) && !ly_is_procedure (a))
      return Smob2<Unpure_pure_container>::make_smob (a, a);
    return Smob2<Unpure_pure_container>::make_smob (a, b);
  }
  int print_smob (SCM, scm_print_state *) const;
};

#endif /* UNPURE_PURE_CONTAINER_HH */
