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

#include "pitch.hh"

LY_DEFINE (ly_pitch_transpose, "ly:pitch-transpose", 2, 0, 0,
           (SCM p, SCM delta),
           R"(
Transpose pitch@tie{}@var{p} by the amount @var{delta}, where @var{delta} is
relative to middle@tie{}C.
           )")
{
  auto *const t = LY_ASSERT_SMOB (Pitch, p, 1);
  auto *const d = LY_ASSERT_SMOB (Pitch, delta, 2);

  return t->transposed (*d).smobbed_copy ();
}

LY_DEFINE (ly_make_pitch, "ly:make-pitch", 2, 1, 0,
           (SCM octave, SCM note, SCM alter),
           R"(
Make a pitch.  @var{octave} is specified by an integer, zero for the octave
containing middle@tie{}C.  @var{note} is a number indexing the global default
scale, with 0 corresponding to pitch@tie{}C and 6 usually corresponding to
pitch@tie{}B.  Optional @var{alter} is a rational number of 200-cent whole
tones for alteration.
           )")

{
  LY_ASSERT_TYPE (scm_is_integer, octave, 1);
  LY_ASSERT_TYPE (scm_is_integer, note, 2);
  if (SCM_UNBNDP (alter))
    alter = SCM_INUM0;
  LY_ASSERT_TYPE (is_scm<Rational>, alter, 3);

  Pitch p (from_scm<int> (octave), from_scm<int> (note),
           from_scm<Rational> (alter));

  return p.smobbed_copy ();
}

LY_DEFINE (ly_pitch_negate, "ly:pitch-negate", 1, 0, 0, (SCM p),
           R"(
Negate pitch@tie{}@var{p}.
           )")
{
  auto *const pp = LY_ASSERT_SMOB (Pitch, p, 1);
  return pp->negated ().smobbed_copy ();
}

LY_DEFINE (ly_pitch_steps, "ly:pitch-steps", 1, 0, 0, (SCM p),
           R"(
Number of steps counted from middle@tie{}C of the pitch@tie{}@var{p}.
           )")
{
  auto *const pp = LY_ASSERT_SMOB (Pitch, p, 1);
  return to_scm (pp->steps ());
}

LY_DEFINE (ly_pitch_octave, "ly:pitch-octave", 1, 0, 0, (SCM pp),
           R"(
Extract the octave from pitch@tie{}@var{pp}.
           )")
{
  auto *const p = LY_ASSERT_SMOB (Pitch, pp, 1);
  int q = p->get_octave ();
  return to_scm (q);
}

LY_DEFINE (ly_pitch_alteration, "ly:pitch-alteration", 1, 0, 0, (SCM pp),
           R"(
Extract the alteration from pitch@tie{}@var{pp}.
           )")
{
  auto *const p = LY_ASSERT_SMOB (Pitch, pp, 1);
  Rational q = p->get_alteration ();

  return to_scm (q);
}

LY_DEFINE (ly_pitch_notename, "ly:pitch-notename", 1, 0, 0, (SCM pp),
           R"(
Extract the note name from pitch @var{pp}.
           )")
{
  auto *const p = LY_ASSERT_SMOB (Pitch, pp, 1);
  int q = p->get_notename ();
  return to_scm (q);
}

LY_DEFINE (ly_pitch_tones, "ly:pitch-tones", 1, 0, 0, (SCM pp),
           R"(
Calculate the number of tones of pitch @var{pp} from middle@tie{}C as a
rational number.
           )")
{
  auto *const p = LY_ASSERT_SMOB (Pitch, pp, 1);
  return to_scm (p->tone_pitch ());
}

LY_DEFINE (ly_pitch_quartertones, "ly:pitch-quartertones", 1, 0, 0, (SCM pp),
           R"(
Calculate the number of quarter tones of pitch @var{pp} from middle@tie{}C.
           )")
{
  auto *const p = LY_ASSERT_SMOB (Pitch, pp, 1);
  int q = p->rounded_quartertone_pitch ();
  return to_scm (q);
}

LY_DEFINE (ly_pitch_semitones, "ly:pitch-semitones", 1, 0, 0, (SCM pp),
           R"(
Calculate the number of semitones of pitch @var{pp} from middle@tie{}C.
           )")
{
  auto *const p = LY_ASSERT_SMOB (Pitch, pp, 1);
  int q = p->rounded_semitone_pitch ();
  return to_scm (q);
}

LY_DEFINE (ly_pitch_less_p, "ly:pitch<?", 2, 0, 0, (SCM p1, SCM p2),
           R"(
Is @var{p1} lexicographically smaller than @var{p2}?
           )")
{
  auto *const a = LY_ASSERT_SMOB (Pitch, p1, 1);
  auto *const b = LY_ASSERT_SMOB (Pitch, p2, 2);

  if (Pitch::compare (*a, *b) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

LY_DEFINE (ly_pitch_diff, "ly:pitch-diff", 2, 0, 0, (SCM pitch, SCM root),
           R"(
Return pitch @var{delta} such that @var{root} transposed by @var{delta} equals
@var{pitch}.
           )")
{

  auto *const p = LY_ASSERT_SMOB (Pitch, pitch, 1);
  auto *const r = LY_ASSERT_SMOB (Pitch, root, 2);

  return pitch_interval (*r, *p).smobbed_copy ();
}

LY_DEFINE (ly_set_middle_C_x, "ly:set-middle-C!", 1, 0, 0, (SCM context),
           R"(
Set the @code{middleCPosition} variable in @var{context} based on the variables
@code{middleCClefPosition} and @code{middleCOffset}.
           )")
{
  auto *const c = LY_ASSERT_SMOB (Context, context, 1);
  set_middle_C (c);
  return SCM_UNSPECIFIED;
}
