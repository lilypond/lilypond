/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

/* TODO: add optional factor argument. */
LY_DEFINE (ly_make_moment, "ly:make-moment",
           1, 3, 0, (SCM m, SCM g, SCM gn, SCM gd),
           "Create the moment with rational main timing @var{m},"
           " and optional grace timing @var{g}.\n"
           "\n"
           "A @dfn{moment} is a point in musical time.  It consists of"
           " a pair of rationals (@var{m},@tie{}@var{g}), where @var{m} is"
           " the timing for the main notes, and @var{g} the timing for"
           " grace notes.  In absence of grace notes, @var{g}@tie{}is zero.\n"
           "\n"
           "For compatibility reasons, it is possible to write two"
           " numbers specifying numerator and denominator instead of"
           " the rationals.  These forms cannot be mixed, and the two-"
           "argument form is disambiguated by the sign of the second"
           " argument: if it is positive, it can only be a denominator"
           " and not a grace timing."
          )
{
  LY_ASSERT_TYPE (ly_is_rational, m, 1);
  if (SCM_UNBNDP (g))
    return Moment (ly_scm2rational (m)).smobbed_copy ();

  if (SCM_UNBNDP (gn))
    {
      LY_ASSERT_TYPE (ly_is_rational, g, 2);
      if (scm_is_true (scm_positive_p (g)))
        {
          LY_ASSERT_TYPE (scm_is_integer, m, 1);
          LY_ASSERT_TYPE (scm_is_integer, g, 2);
          return Moment (Rational (scm_to_int64 (m),
                                   scm_to_int64 (g))).smobbed_copy ();
        }
      return Moment (ly_scm2rational (m),
                     ly_scm2rational (g)).smobbed_copy ();
    }

  LY_ASSERT_TYPE (scm_is_integer, m, 1);
  LY_ASSERT_TYPE (scm_is_integer, g, 2);
  LY_ASSERT_TYPE (scm_is_integer, gn, 3);
  I64 grace_num = scm_to_int64 (gn);
  I64 grace_den = 1;
  if (!SCM_UNBNDP (gd))
    {
      LY_ASSERT_TYPE (scm_is_integer, gd, 4);
      grace_den = scm_to_int64 (gd);
    }

  return Moment (Rational (scm_to_int64 (m), scm_to_int64 (g)),
                 Rational (grace_num, grace_den)).smobbed_copy ();
}

LY_DEFINE (ly_moment_sub, "ly:moment-sub",
           2, 0, 0, (SCM a, SCM b),
           "Subtract two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1);
  LY_ASSERT_SMOB (Moment, b, 2);

  Moment *ma = unsmob<Moment> (a);
  Moment *mb = unsmob<Moment> (b);

  return (*ma - *mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_add, "ly:moment-add",
           2, 0, 0, (SCM a, SCM b),
           "Add two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1);
  LY_ASSERT_SMOB (Moment, b, 2);

  Moment *ma = unsmob<Moment> (a);
  Moment *mb = unsmob<Moment> (b);

  return (*ma + *mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_mul, "ly:moment-mul",
           2, 0, 0, (SCM a, SCM b),
           "Multiply two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1);
  LY_ASSERT_SMOB (Moment, b, 2); // TODO: should be Rational

  Moment *ma = unsmob<Moment> (a);
  Moment *mb = unsmob<Moment> (b);
  return (*ma * mb->main_part_).smobbed_copy ();
}

LY_DEFINE (ly_moment_div, "ly:moment-div",
           2, 0, 0, (SCM a, SCM b),
           "Divide two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1);
  LY_ASSERT_SMOB (Moment, b, 2); // TODO: should be Rational

  Moment *ma = unsmob<Moment> (a);
  Moment *mb = unsmob<Moment> (b);

  return (*ma / mb->main_part_).smobbed_copy ();
}

LY_DEFINE (ly_moment_mod, "ly:moment-mod",
           2, 0, 0, (SCM a, SCM b),
           "Modulo of two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1);
  LY_ASSERT_SMOB (Moment, b, 2); // TODO: should be Rational

  Moment *ma = unsmob<Moment> (a);
  Moment *mb = unsmob<Moment> (b);
  return (*ma % mb->main_part_).smobbed_copy ();
}

LY_DEFINE (ly_moment_grace, "ly:moment-grace",
           1, 0, 0, (SCM mom),
           "Extract grace timing as a rational number from @var{mom}.")
{
  LY_ASSERT_SMOB (Moment, mom, 1);

  return ly_rational2scm (unsmob<Moment> (mom)->grace_part_);
}

LY_DEFINE (ly_moment_grace_numerator, "ly:moment-grace-numerator",
           1, 0, 0, (SCM mom),
           "Extract numerator from grace timing.")
{
  LY_ASSERT_SMOB (Moment, mom, 1);

  Moment *ma = unsmob<Moment> (mom);

  return scm_from_int64 (ma->grace_part_.numerator ());
}

LY_DEFINE (ly_moment_grace_denominator, "ly:moment-grace-denominator",
           1, 0, 0, (SCM mom),
           "Extract denominator from grace timing.")
{
  LY_ASSERT_SMOB (Moment, mom, 1);
  Moment *ma = unsmob<Moment> (mom);

  return scm_from_int64 (ma->grace_part_.denominator ());
}

LY_DEFINE (ly_moment_main, "ly:moment-main",
           1, 0, 0, (SCM mom),
           "Extract main timing as a rational number from @var{mom}.")
{
  LY_ASSERT_SMOB (Moment, mom, 1);

  return ly_rational2scm (unsmob<Moment> (mom)->main_part_);
}

LY_DEFINE (ly_moment_main_numerator, "ly:moment-main-numerator",
           1, 0, 0, (SCM mom),
           "Extract numerator from main timing.")
{
  LY_ASSERT_SMOB (Moment, mom, 1);
  Moment *ma = unsmob<Moment> (mom);

  return scm_from_int64 (ma->main_part_.numerator ());
}

LY_DEFINE (ly_moment_main_denominator, "ly:moment-main-denominator",
           1, 0, 0, (SCM mom),
           "Extract denominator from main timing.")
{
  LY_ASSERT_SMOB (Moment, mom, 1);
  Moment *ma = unsmob<Moment> (mom);

  return scm_from_int64 (ma->main_part_.denominator ());
}

LY_DEFINE (ly_moment_less_p, "ly:moment<?",
           2, 0, 0, (SCM a, SCM b),
           "Compare two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1);
  LY_ASSERT_SMOB (Moment, b, 2);

  Moment *ma = unsmob<Moment> (a);
  Moment *mb = unsmob<Moment> (b);

  return ly_bool2scm (*ma < *mb);
}

