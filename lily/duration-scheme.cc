/*
  duration.cc -- implement Duration

  source file of the LilyPond music typesetter

  (c) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "duration.hh"
#include "misc.hh"

MAKE_SCHEME_CALLBACK (Duration, less_p, 2);
SCM
Duration::less_p (SCM p1, SCM p2)
{
  Duration *a = unsmob_duration (p1);
  Duration *b = unsmob_duration (p2);

  if (compare (*a, *b) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

LY_DEFINE (ly_duration_less_p, "ly:duration<?",
	   2, 0, 0, (SCM p1, SCM p2),
	   "Is @var{p1} shorter than @var{p2}?")
{
  LY_ASSERT_SMOB (Duration, p1, 1);
  LY_ASSERT_SMOB (Duration, p2, 2);

  Duration *a = unsmob_duration (p1);
  Duration *b = unsmob_duration (p2);

  if (Duration::compare (*a, *b) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

LY_DEFINE (ly_make_duration, "ly:make-duration",
	   1, 3, 0, (SCM length, SCM dotcount, SCM num, SCM den),
	   "@var{length} is the negative logarithm (base 2) of the duration:"
	   " 1@tie{}is a half note, 2@tie{}is a quarter note, 3@tie{}is an"
	   " eighth note, etc.  The number of dots after the note is given by"
	   " the optional argument @var{dotcount}.\n"
	   "\n"
	   "The duration factor is optionally given by @var{num} and"
	   " @var{den}.\n"
	   "\n"
	   "A duration is a musical duration, i.e., a length of time"
	   " described by a power of two (whole, half, quarter, etc.) and a"
	   " number of augmentation dots.")
{
  LY_ASSERT_TYPE (scm_is_integer, length, 1);

  int dots = 0;
  if (dotcount != SCM_UNDEFINED)
    {
      LY_ASSERT_TYPE (scm_is_integer, dotcount, 2);
      dots = scm_to_int (dotcount);
    }

  bool compress = false;
  if (num != SCM_UNDEFINED)
    {
      LY_ASSERT_TYPE (scm_is_number, num, 3);
      compress = true;
    }
  else
    num = scm_from_int (1);

  if (den != SCM_UNDEFINED)
    {
      LY_ASSERT_TYPE (scm_is_number, den, 4);
      compress = true;
    }
  else
    den = scm_from_int (1);

  Duration p (scm_to_int (length), dots);
  if (compress)
    p = p.compressed (Rational (scm_to_int (num), scm_to_int (den)));

  return p.smobbed_copy ();
}

LY_DEFINE (ly_duration_log, "ly:duration-log",
	   1, 0, 0, (SCM dur),
	   "Extract the duration log from @var{dur}.")
{
  LY_ASSERT_SMOB (Duration, dur, 1);
  return scm_from_int (unsmob_duration (dur)->duration_log ());
}

LY_DEFINE (ly_duration_dot_count, "ly:duration-dot-count",
	   1, 0, 0, (SCM dur),
	   "Extract the dot count from @var{dur}.")
{
  LY_ASSERT_SMOB (Duration, dur, 1);
  return scm_from_int (unsmob_duration (dur)->dot_count ());
}

LY_DEFINE (ly_intlog2, "ly:intlog2",
	   1, 0, 0, (SCM d),
	   "The 2-logarithm of 1/@var{d}.")
{
  LY_ASSERT_TYPE (scm_is_number, d, 1);
  int log = intlog2 (scm_to_int (d));
  return scm_from_int (log);
}

LY_DEFINE (ly_duration_length, "ly:duration-length",
	   1, 0, 0, (SCM dur),
	   "The length of the duration as a @code{moment}.")
{
  LY_ASSERT_SMOB (Duration, dur, 1);
  return Moment (unsmob_duration (dur)->get_length ()).smobbed_copy ();
}

LY_DEFINE (ly_duration_2_string, "ly:duration->string",
	   1, 0, 0, (SCM dur),
	   "Convert @var{dur} to a string.")
{
  LY_ASSERT_SMOB (Duration, dur, 1);
  return ly_string2scm (unsmob_duration (dur)->to_string ());
}

LY_DEFINE (ly_duration_factor, "ly:duration-factor",
	   1, 0, 0, (SCM dur),
	   "Extract the compression factor from @var{dur}."
	   "  Return it as a pair.")
{
  LY_ASSERT_SMOB (Duration, dur, 1);
  Rational r = unsmob_duration (dur)->factor ();
  return scm_cons (scm_from_int64 (r.num ()), scm_from_int64 (r.den ()));
}
