/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
	   2, 2, 0, (SCM n, SCM d, SCM gn, SCM gd),
	   "Create the rational number with main timing @var{n}/@var{d},"
	   " and optional grace timing @var{gn}/@var{gd}.\n"
	   "\n"
	   "A @dfn{moment} is a point in musical time.  It consists of"
	   " a pair of rationals (@var{m},@tie{}@var{g}), where @var{m} is"
	   " the timing for the main notes, and @var{g} the timing for"
	   " grace notes.  In absence of grace notes, @var{g}@tie{}is zero.")
{
  LY_ASSERT_TYPE (scm_is_integer, n, 1);
  LY_ASSERT_TYPE (scm_is_integer, d, 2);

  int grace_num = 0;
  if (gn != SCM_UNDEFINED)
    {
      LY_ASSERT_TYPE (scm_is_integer, gn, 3);
      grace_num = scm_to_int (gn);
    }

  int grace_den = 1;
  if (gd != SCM_UNDEFINED)
    {
      LY_ASSERT_TYPE (scm_is_integer, gd, 4);
      grace_den = scm_to_int (gd);
    }

  return Moment (Rational (scm_to_int (n), scm_to_int (d)),
		 Rational (grace_num, grace_den)).smobbed_copy ();
}

LY_DEFINE (ly_moment_sub, "ly:moment-sub",
	   2, 0, 0, (SCM a, SCM b),
	   "Subtract two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1); 
  LY_ASSERT_SMOB (Moment, b, 2);
  
  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);

  return (*ma - *mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_add, "ly:moment-add",
	   2, 0, 0, (SCM a, SCM b),
	   "Add two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1); 
  LY_ASSERT_SMOB (Moment, b, 2); 

  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);

  return (*ma + *mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_mul, "ly:moment-mul",
	   2, 0, 0, (SCM a, SCM b),
	   "Multiply two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1); 
  LY_ASSERT_SMOB (Moment, b, 2); 

  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);
  return (*ma * * mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_div, "ly:moment-div",
	   2, 0, 0, (SCM a, SCM b),
	   "Divide two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1); 
  LY_ASSERT_SMOB (Moment, b, 2); 

  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);
  
  return (*ma / * mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_mod, "ly:moment-mod",
	   2, 0, 0, (SCM a, SCM b),
	   "Modulo of two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1); 
  LY_ASSERT_SMOB (Moment, b, 2); 
  
  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);
  return (*ma % * mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_grace_numerator, "ly:moment-grace-numerator",
	   1, 0, 0, (SCM mom),
	   "Extract numerator from grace timing.")
{
  LY_ASSERT_SMOB (Moment, mom, 1);

  Moment *ma = unsmob_moment (mom);

  return scm_from_int64 (ma->grace_part_.numerator ());
}

LY_DEFINE (ly_moment_grace_denominator, "ly:moment-grace-denominator",
	   1, 0, 0, (SCM mom),
	   "Extract denominator from grace timing.")
{
  LY_ASSERT_SMOB (Moment, mom, 1);
  Moment *ma = unsmob_moment (mom);

  return scm_from_int64 (ma->grace_part_.denominator ());
}
LY_DEFINE (ly_moment_main_numerator, "ly:moment-main-numerator",
	   1, 0, 0, (SCM mom),
	   "Extract numerator from main timing.")
{
  LY_ASSERT_SMOB (Moment, mom, 1);
  Moment *ma = unsmob_moment (mom);

  return scm_from_int64 (ma->main_part_.numerator ());
}

LY_DEFINE (ly_moment_main_denominator, "ly:moment-main-denominator",
	   1, 0, 0, (SCM mom),
	   "Extract denominator from main timing.")
{
  LY_ASSERT_SMOB (Moment, mom, 1);
  Moment *ma = unsmob_moment (mom);

  return scm_from_int64 (ma->main_part_.denominator ());
}

LY_DEFINE (ly_moment_less_p, "ly:moment<?",
	   2, 0, 0, (SCM a, SCM b),
	   "Compare two moments.")
{
  LY_ASSERT_SMOB (Moment, a, 1); 
  LY_ASSERT_SMOB (Moment, b, 2); 
  
  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);

  return ly_bool2scm (*ma < *mb);
}

