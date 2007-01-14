/*
  moment.cc -- implement Moment bindings

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "moment.hh"

/* TODO: add optional factor argument. */
LY_DEFINE (ly_make_moment, "ly:make-moment",
	   2, 2, 0, (SCM n, SCM d, SCM gn, SCM gd),
	   "Create the rational number with main timing @var{n}/@var{d}, "
	   "and optional grace timin @var{gn}/@var{gd}.\n"
	   "\n"
	   "\n"
	   "Moment is a point in musical time.  "
	   "It is consists of a pair of rationals (@var{m}, @var{g}), "
	   "where @var{m} is the timing for the main\n"
	   "notes, and @var{g} the timing for grace notes.  "
	   "In absence of grace notes, @var{g} is zero.\n")
{
  SCM_ASSERT_TYPE (scm_is_integer (n), n, SCM_ARG1, __FUNCTION__, "integer");
  SCM_ASSERT_TYPE (scm_is_integer (d), d, SCM_ARG2, __FUNCTION__, "integer");

  int grace_num = 0;
  if (gn != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (scm_is_integer (gn), gn, SCM_ARG3, __FUNCTION__, "integer");
      grace_num = scm_to_int (gn);
    }

  int grace_den = 1;
  if (gd != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (scm_is_integer (gd), gd, SCM_ARG4, __FUNCTION__, "integer");
      grace_den = scm_to_int (gd);
    }

  return Moment (Rational (scm_to_int (n), scm_to_int (d)),
		 Rational (grace_num, grace_den)).smobbed_copy ();
}

LY_DEFINE (ly_sub_moment, "ly:moment-sub",
	   2, 0, 0, (SCM a, SCM b),
	   "Subtract two moments.")
{
  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);
  SCM_ASSERT_TYPE (ma, a, SCM_ARG1, __FUNCTION__, "moment");
  SCM_ASSERT_TYPE (mb, b, SCM_ARG2, __FUNCTION__, "moment");
  return (*ma - *mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_add, "ly:moment-add",
	   2, 0, 0, (SCM a, SCM b),
	   "Add two moments.")
{
  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);
  SCM_ASSERT_TYPE (ma, a, SCM_ARG1, __FUNCTION__, "moment");
  SCM_ASSERT_TYPE (mb, b, SCM_ARG2, __FUNCTION__, "moment");
  return (*ma + *mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_mul, "ly:moment-mul",
	   2, 0, 0, (SCM a, SCM b),
	   "Multiply two moments.")
{
  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);
  SCM_ASSERT_TYPE (ma, a, SCM_ARG1, __FUNCTION__, "moment");
  SCM_ASSERT_TYPE (mb, b, SCM_ARG2, __FUNCTION__, "moment");
  return (*ma * * mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_div, "ly:moment-div",
	   2, 0, 0, (SCM a, SCM b),
	   "Divide two moments.")
{
  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);
  SCM_ASSERT_TYPE (ma, a, SCM_ARG1, __FUNCTION__, "moment");
  SCM_ASSERT_TYPE (mb, b, SCM_ARG2, __FUNCTION__, "moment");
  return (*ma / * mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_mod, "ly:moment-mod",
	   2, 0, 0, (SCM a, SCM b),
	   "Modulo of two moments.")
{
  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);
  SCM_ASSERT_TYPE (ma, a, SCM_ARG1, __FUNCTION__, "moment");
  SCM_ASSERT_TYPE (mb, b, SCM_ARG2, __FUNCTION__, "moment");
  return (*ma % * mb).smobbed_copy ();
}

LY_DEFINE (ly_moment_grace_numerator, "ly:moment-grace-numerator",
	   1, 0, 0, (SCM mom),
	   "Extract numerator from grace timing.")
{
  Moment *ma = unsmob_moment (mom);
  SCM_ASSERT_TYPE (ma, mom, SCM_ARG1, __FUNCTION__, "moment");

  return scm_from_int (ma->grace_part_.numerator ());
}

LY_DEFINE (ly_moment_grace_denominator, "ly:moment-grace-denominator",
	   1, 0, 0, (SCM mom),
	   "Extract denominator from grace timing.")
{
  Moment *ma = unsmob_moment (mom);
  SCM_ASSERT_TYPE (ma, mom, SCM_ARG1, __FUNCTION__, "moment");

  return scm_from_int (ma->grace_part_.denominator ());
}
LY_DEFINE (ly_moment_main_numerator, "ly:moment-main-numerator",
	   1, 0, 0, (SCM mom),
	   "Extract numerator from main timing.")
{
  Moment *ma = unsmob_moment (mom);
  SCM_ASSERT_TYPE (ma, mom, SCM_ARG1, __FUNCTION__, "moment");

  return scm_from_int (ma->main_part_.numerator ());
}

LY_DEFINE (ly_moment_main_denominator, "ly:moment-main-denominator",
	   1, 0, 0, (SCM mom),
	   "Extract denominator from main timing.")
{
  Moment *ma = unsmob_moment (mom);
  SCM_ASSERT_TYPE (ma, mom, SCM_ARG1, __FUNCTION__, "moment");

  return scm_from_int (ma->main_part_.denominator ());
}

LY_DEFINE (ly_moment_less_p, "ly:moment<?",
	   2, 0, 0, (SCM a, SCM b),
	   "Compare two moments.")
{
  Moment *ma = unsmob_moment (a);
  Moment *mb = unsmob_moment (b);
  SCM_ASSERT_TYPE (ma, a, SCM_ARG1, __FUNCTION__, "moment");
  SCM_ASSERT_TYPE (mb, b, SCM_ARG2, __FUNCTION__, "moment");
  return ly_bool2scm (*ma < *mb);
}

