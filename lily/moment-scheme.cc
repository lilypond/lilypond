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

#include "moment.hh"

#include <cstdint>

/* TODO: add optional factor argument. */
LY_DEFINE (ly_make_moment, "ly:make-moment", 1, 3, 0,
           (SCM m, SCM g, SCM gn, SCM gd),
           R"(
Create a moment with rational main timing @var{m}, and optional grace timing
@var{g}.

A @dfn{moment} is a point in musical time.  It consists of a pair of rationals
@w{(@var{m}, @var{g})}, where @var{m} is the timing for the main notes, and
@var{g} the timing for grace notes.  In absence of grace notes, @var{g}@tie{}is
zero.

For compatibility reasons, it is possible to write two numbers specifying
numerator and denominator instead of the rationals.  These forms cannot be
mixed, and the two-argument form is disambiguated by the sign of the second
argument: if it is positive, it can only be a denominator and not a grace
timing.
           )")
{
  LY_ASSERT_TYPE (is_scm<Rational>, m, 1);
  if (SCM_UNBNDP (g))
    return to_scm (Moment (from_scm<Rational> (m)));

  if (SCM_UNBNDP (gn))
    {
      LY_ASSERT_TYPE (is_scm<Rational>, g, 2);
      if (scm_is_true (scm_positive_p (g)))
        {
          LY_ASSERT_TYPE (scm_is_integer, m, 1);
          LY_ASSERT_TYPE (scm_is_integer, g, 2);
          return to_scm (
            Moment (Rational (scm_to_int64 (m), scm_to_int64 (g))));
        }
      return to_scm (Moment (from_scm<Rational> (m), from_scm<Rational> (g)));
    }

  LY_ASSERT_TYPE (scm_is_integer, m, 1);
  LY_ASSERT_TYPE (scm_is_integer, g, 2);
  LY_ASSERT_TYPE (scm_is_integer, gn, 3);
  int64_t grace_num = scm_to_int64 (gn);
  int64_t grace_den = 1;
  if (!SCM_UNBNDP (gd))
    {
      LY_ASSERT_TYPE (scm_is_integer, gd, 4);
      grace_den = scm_to_int64 (gd);
    }

  return to_scm (Moment (Rational (scm_to_int64 (m), scm_to_int64 (g)),
                         Rational (grace_num, grace_den)));
}

LY_DEFINE (ly_moment_sub, "ly:moment-sub", 2, 0, 0, (SCM a, SCM b),
           R"(
Subtract two moments.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, a, 1);
  auto *const mb = LY_ASSERT_SMOB (Moment, b, 2);

  return to_scm (*ma - *mb);
}

LY_DEFINE (ly_moment_add, "ly:moment-add", 2, 0, 0, (SCM a, SCM b),
           R"(
Add two moments.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, a, 1);
  auto *const mb = LY_ASSERT_SMOB (Moment, b, 2);

  return to_scm (*ma + *mb);
}

LY_DEFINE (ly_moment_mul, "ly:moment-mul", 2, 0, 0, (SCM a, SCM b),
           R"(
Multiply two moments.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, a, 1);
  auto *const mb = LY_ASSERT_SMOB (Moment, b, 2); // TODO: should be Rational

  return to_scm (*ma * mb->main_part_);
}

LY_DEFINE (ly_moment_div, "ly:moment-div", 2, 0, 0, (SCM a, SCM b),
           R"(
Divide two moments.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, a, 1);
  auto *const mb = LY_ASSERT_SMOB (Moment, b, 2); // TODO: should be Rational

  return to_scm (*ma / mb->main_part_);
}

LY_DEFINE (ly_moment_mod, "ly:moment-mod", 2, 0, 0, (SCM a, SCM b),
           R"(
Modulo of two moments.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, a, 1);
  auto *const mb = LY_ASSERT_SMOB (Moment, b, 2); // TODO: should be Rational

  return to_scm (*ma % mb->main_part_);
}

LY_DEFINE (ly_moment_grace, "ly:moment-grace", 1, 0, 0, (SCM mom),
           R"(
Extract grace timing as a rational number from @var{mom}.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, mom, 1);

  return to_scm (ma->grace_part_);
}

LY_DEFINE (ly_moment_grace_numerator, "ly:moment-grace-numerator", 1, 0, 0,
           (SCM mom),
           R"(
Extract numerator from grace timing.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, mom, 1);

  return to_scm (ma->grace_part_.numerator ());
}

LY_DEFINE (ly_moment_grace_denominator, "ly:moment-grace-denominator", 1, 0, 0,
           (SCM mom),
           R"(
Extract denominator from grace timing.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, mom, 1);

  return to_scm (ma->grace_part_.denominator ());
}

LY_DEFINE (ly_moment_main, "ly:moment-main", 1, 0, 0, (SCM mom),
           R"(
Extract main timing as a rational number from @var{mom}.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, mom, 1);

  return to_scm (ma->main_part_);
}

LY_DEFINE (ly_moment_main_numerator, "ly:moment-main-numerator", 1, 0, 0,
           (SCM mom),
           R"(
Extract numerator from main timing.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, mom, 1);

  return to_scm (ma->main_part_.numerator ());
}

LY_DEFINE (ly_moment_main_denominator, "ly:moment-main-denominator", 1, 0, 0,
           (SCM mom),
           R"(
Extract denominator from main timing.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, mom, 1);

  return to_scm (ma->main_part_.denominator ());
}

LY_DEFINE (ly_moment_less_p, "ly:moment<?", 2, 0, 0, (SCM a, SCM b),
           R"(
Compare two moments.
           )")
{
  auto *const ma = LY_ASSERT_SMOB (Moment, a, 1);
  auto *const mb = LY_ASSERT_SMOB (Moment, b, 2);

  return to_scm (*ma < *mb);
}
