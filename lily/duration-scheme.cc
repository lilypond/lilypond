/*
  duration.cc -- implement Duration

  source file of the LilyPond music typesetter

  (c) 1997--2007 Jan Nieuwenhuizen <janneke@gnu.org>
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
  Duration *a = unsmob_duration (p1);
  Duration *b = unsmob_duration (p2);

  SCM_ASSERT_TYPE (a, p1, SCM_ARG1, __FUNCTION__, "Duration");
  SCM_ASSERT_TYPE (b, p2, SCM_ARG2, __FUNCTION__, "Duration");

  if (Duration::compare (*a, *b) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

LY_DEFINE (ly_make_duration, "ly:make-duration",
	   1, 3, 0, (SCM length, SCM dotcount, SCM num, SCM den),
	   "@var{length} is the negative logarithm (base 2) of the duration:\n"
	   "1 is a half note, 2 is a quarter note, 3 is an eighth\n"
	   "note, etc.  The number of dots after the note is given by\n"
	   "the optional argument @var{dotcount}.\n"
	   "\n"
	   "The duration factor is optionally given by @var{num}\n"
	   "and @var{den}.\n\n"
	   "A duration is a musical duration, "
	   "i.e. a length of time described by a power of two "
	   "(whole, half, quarter, etc.) and a number of augmentation\n"
	   "dots. \n")
{
  SCM_ASSERT_TYPE (scm_is_integer (length),
		   length, SCM_ARG1, __FUNCTION__, "integer");

  int dots = 0;
  if (dotcount != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (scm_is_integer (dotcount),
		       dotcount, SCM_ARG2, __FUNCTION__, "integer");
      dots = scm_to_int (dotcount);
    }

  bool compress = false;
  if (num != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (scm_is_number (num), num, SCM_ARG3, __FUNCTION__, "integer");
      compress = true;
    }
  else
    num = scm_from_int (1);

  if (den != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (scm_is_number (den), den, SCM_ARG4, __FUNCTION__, "integer");
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
	   "Extract the duration log from @var{dur}")
{
  SCM_ASSERT_TYPE (unsmob_duration (dur), dur, SCM_ARG1, __FUNCTION__, "duration");
  return scm_from_int (unsmob_duration (dur)->duration_log ());
}

LY_DEFINE (ly_duration_dot_count, "ly:duration-dot-count",
	   1, 0, 0, (SCM dur),
	   "Extract the dot count from @var{dur}")
{
  SCM_ASSERT_TYPE (unsmob_duration (dur), dur, SCM_ARG1, __FUNCTION__, "duration");
  return scm_from_int (unsmob_duration (dur)->dot_count ());
}

LY_DEFINE (ly_intlog2, "ly:intlog2",
	   1, 0, 0, (SCM d),
	   "The 2-logarithm of 1/@var{d}.")
{
  SCM_ASSERT_TYPE (scm_is_number (d), d, SCM_ARG1, __FUNCTION__, "integer");
  int log = intlog2 (scm_to_int (d));
  return scm_from_int (log);
}

LY_DEFINE (ly_duration_length, "ly:duration-length",
	   1, 0, 0, (SCM dur),
	   "The length of the duration as a Moment.")
{
  SCM_ASSERT_TYPE (unsmob_duration (dur), dur, SCM_ARG1, __FUNCTION__, "duration");
  return Moment (unsmob_duration (dur)->get_length ()).smobbed_copy ();
}

LY_DEFINE (ly_duration_factor, "ly:duration-factor",
	   1, 0, 0, (SCM dur),
	   "Extract the compression factor from @var{dur}. Return as a pair.")
{
  SCM_ASSERT_TYPE (unsmob_duration (dur), dur, SCM_ARG1, __FUNCTION__, "duration");
  Rational r = unsmob_duration (dur)->factor ();
  return scm_cons (scm_from_int (r.num ()), scm_from_int (r.den ()));
}
