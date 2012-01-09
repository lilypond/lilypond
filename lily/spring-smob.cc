/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  return a == b ? SCM_BOOL_T : SCM_BOOL_F;
}

LY_DEFINE (ly_make_spring, "ly:make-spring",
           2, 0, 0, (SCM ideal, SCM min_dist),
           "Make a spring.  @var{ideal} is the ideal distance of the"
           " spring, and @var{min-dist} is the minimum distance.")
{
  LY_ASSERT_TYPE (scm_is_number, ideal, 1);
  LY_ASSERT_TYPE (scm_is_number, min_dist, 2);

  Spring s (scm_to_double (ideal), scm_to_double (min_dist));

  return s.smobbed_copy ();
}

LY_DEFINE (ly_spring_set_inverse_compress_strength_x, "ly:spring-set-inverse-compress-strength!",
           2, 0, 0, (SCM spring, SCM strength),
           "Set the inverse compress @var{strength} of @var{spring}.")
{
  LY_ASSERT_SMOB (Spring, spring, 1);
  LY_ASSERT_TYPE (scm_is_number, strength, 2);

  Spring *s = unsmob_spring (spring);
  s->set_inverse_compress_strength (scm_to_double (strength));
  return s->smobbed_copy ();
}

LY_DEFINE (ly_spring_set_inverse_stretch_strength_x, "ly:spring-set-inverse-stretch-strength!",
           2, 0, 0, (SCM spring, SCM strength),
           "Set the inverse stretch @var{strength} of @var{spring}.")
{
  LY_ASSERT_SMOB (Spring, spring, 1);
  LY_ASSERT_TYPE (scm_is_number, strength, 2);

  Spring *s = unsmob_spring (spring);
  s->set_inverse_stretch_strength (scm_to_double (strength));
  return s->smobbed_copy ();
}

IMPLEMENT_TYPE_P (Spring, "ly:spring?");
