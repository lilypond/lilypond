/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

SCM
Spring::equal_p (SCM a, SCM b)
{
  // TODO SCM: This could be made simpler.
  return scm_is_eq (a, b) ? SCM_BOOL_T : SCM_BOOL_F;
}

LY_DEFINE (ly_make_spring, "ly:make-spring", 2, 0, 0, (SCM ideal, SCM min_dist),
           R"(
Make a spring.  @var{ideal} is the ideal distance of the spring, and
@var{min-dist} is the minimum distance.
           )")
{
  LY_ASSERT_TYPE (scm_is_number, ideal, 1);
  LY_ASSERT_TYPE (scm_is_number, min_dist, 2);

  Spring s (from_scm<double> (ideal), from_scm<double> (min_dist));

  return s.smobbed_copy ();
}

LY_DEFINE (ly_spring_set_inverse_compress_strength_x,
           "ly:spring-set-inverse-compress-strength!", 2, 0, 0,
           (SCM spring, SCM strength),
           R"(
Set the inverse compress @var{strength} of @var{spring}.
           )")
{
  auto *const s = LY_ASSERT_SMOB (Spring, spring, 1);
  LY_ASSERT_TYPE (scm_is_number, strength, 2);

  s->set_inverse_compress_strength (from_scm<double> (strength));
  return s->smobbed_copy ();
}

LY_DEFINE (ly_spring_set_inverse_stretch_strength_x,
           "ly:spring-set-inverse-stretch-strength!", 2, 0, 0,
           (SCM spring, SCM strength),
           R"(
Set the inverse stretch @var{strength} of @var{spring}.
           )")
{
  auto *const s = LY_ASSERT_SMOB (Spring, spring, 1);
  LY_ASSERT_TYPE (scm_is_number, strength, 2);

  s->set_inverse_stretch_strength (from_scm<double> (strength));
  return s->smobbed_copy ();
}

const char *const Spring::type_p_name_ = "ly:spring?";
