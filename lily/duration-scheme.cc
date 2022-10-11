/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "duration.hh"

#include "misc.hh"
#include "moment.hh"

MAKE_SCHEME_CALLBACK (Duration, less_p, "ly:duration::less?", 2);
SCM
Duration::less_p (SCM p1, SCM p2)
{
  Duration *a = unsmob<Duration> (p1);
  Duration *b = unsmob<Duration> (p2);

  if (compare (*a, *b) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

LY_DEFINE (ly_duration_less_p, "ly:duration<?", 2, 0, 0, (SCM p1, SCM p2),
           R"(
Is @var{p1} shorter than @var{p2}?
           )")
{
  auto *const a = LY_ASSERT_SMOB (Duration, p1, 1);
  auto *const b = LY_ASSERT_SMOB (Duration, p2, 2);

  if (Duration::compare (*a, *b) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

LY_DEFINE (ly_make_duration, "ly:make-duration", 1, 3, 0,
           (SCM length, SCM dotcount, SCM num, SCM den),
           R"(
Make a duration.  @var{length} is the negative logarithm (base@tie{}2) of the
duration: 1@tie{}is a half note, 2@tie{}is a quarter note, 3@tie{}is an eighth
note, etc.  The number of dots after the note is given by the optional argument
@var{dotcount}.

The duration factor is optionally given by integers @var{num} and @var{den},
alternatively by a single rational number.

A duration is a musical duration, i.e., a length of time described by a power
of two (whole, half, quarter, etc.) and a number of augmentation dots.
           )")
{
  LY_ASSERT_TYPE (scm_is_integer, length, 1);

  int dots = 0;
  if (!SCM_UNBNDP (dotcount))
    {
      LY_ASSERT_TYPE (scm_is_integer, dotcount, 2);
      dots = from_scm<int> (dotcount);
    }

  bool compress = false;
  if (!SCM_UNBNDP (num))
    {
      LY_ASSERT_TYPE (is_scm<Rational>, num, 3);
      compress = true;
    }
  else
    num = to_scm (1);

  if (!SCM_UNBNDP (den))
    {
      LY_ASSERT_TYPE (scm_is_integer, den, 4);
      compress = true;
    }
  else
    den = to_scm (1);

  Duration p (from_scm<int> (length), dots);
  if (compress)
    p = p.compressed (from_scm<Rational> (scm_divide (num, den)));

  return p.smobbed_copy ();
}

LY_DEFINE (ly_duration_log, "ly:duration-log", 1, 0, 0, (SCM dur),
           R"(
Extract the duration log from @var{dur}.
           )")
{
  auto *const a = LY_ASSERT_SMOB (Duration, dur, 1);
  return to_scm (a->duration_log ());
}

LY_DEFINE (ly_duration_dot_count, "ly:duration-dot-count", 1, 0, 0, (SCM dur),
           R"(
Extract the dot count from @var{dur}.
           )")
{
  auto *const a = LY_ASSERT_SMOB (Duration, dur, 1);
  return to_scm (a->dot_count ());
}

LY_DEFINE (ly_intlog2, "ly:intlog2", 1, 0, 0, (SCM d),
           R"(
The 2-logarithm of 1/@var{d}.
           )")
{
  LY_ASSERT_TYPE (scm_is_number, d, 1);
  int log = intlog2 (from_scm<int> (d));
  return to_scm (log);
}

LY_DEFINE (ly_duration_length, "ly:duration-length", 1, 0, 0, (SCM dur),
           R"(
The length of the duration as a @code{moment}.
           )")
{
  auto *const a = LY_ASSERT_SMOB (Duration, dur, 1);
  return Moment (a->get_length ()).smobbed_copy ();
}

LY_DEFINE (ly_duration_2_string, "ly:duration->string", 1, 0, 0, (SCM dur),
           R"(
Convert @var{dur} to a string.
           )")
{
  auto *const a = LY_ASSERT_SMOB (Duration, dur, 1);
  return ly_string2scm (a->to_string ());
}

LY_DEFINE (ly_duration_factor, "ly:duration-factor", 1, 0, 0, (SCM dur),
           R"(
Extract the compression factor from @var{dur}.  Return it as a pair.
           )")
{
  auto *const a = LY_ASSERT_SMOB (Duration, dur, 1);
  Rational r = a->factor ();
  return scm_cons (to_scm (r.num ()), to_scm (r.den ()));
}

// This is likely what ly:duration-factor should have been in the
// first place.
LY_DEFINE (ly_duration_scale, "ly:duration-scale", 1, 0, 0, (SCM dur),
           R"(
Extract the compression factor from @var{dur}.  Return it as a rational.
           )")
{
  auto *const a = LY_ASSERT_SMOB (Duration, dur, 1);
  Rational r = a->factor ();

  return to_scm (r);
}

LY_DEFINE (ly_duration_compress, "ly:duration-compress", 2, 0, 0,
           (SCM dur, SCM factor),
           R"(
Compress @var{dur} by rational @var{factor}.
           )")
{
  auto *const a = LY_ASSERT_SMOB (Duration, dur, 1);
  LY_ASSERT_TYPE (is_scm<Rational>, factor, 2);

  return a->compressed (from_scm<Rational> (factor)).smobbed_copy ();
}
