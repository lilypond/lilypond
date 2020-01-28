/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
           "Transpose @var{p} by the amount @var{delta},"
           " where @var{delta} is relative to middle@tie{}C.")
{
  LY_ASSERT_SMOB (Pitch, p, 1);
  LY_ASSERT_SMOB (Pitch, delta, 2);

  Pitch *t = unsmob<Pitch> (p);
  Pitch *d = unsmob<Pitch> (delta);
  return t->transposed (*d).smobbed_copy ();
}

LY_DEFINE (ly_make_pitch, "ly:make-pitch", 2, 1, 0,
           (SCM octave, SCM note, SCM alter),
           "@var{octave} is specified by an integer, zero for the octave"
           " containing middle@tie{}C.  @var{note} is a number indexing the"
           " global default scale, with 0 corresponding to pitch@tie{}C"
           " and 6 usually corresponding to pitch@tie{}B."
           "  Optional @var{alter} is"
           " a rational number of 200-cent whole tones for alteration.")

{
  LY_ASSERT_TYPE (scm_is_integer, octave, 1);
  LY_ASSERT_TYPE (scm_is_integer, note, 2);
  if (SCM_UNBNDP (alter))
    alter = SCM_INUM0;
  LY_ASSERT_TYPE (scm_is_rational, alter, 3);

  Pitch p (scm_to_int (octave), scm_to_int (note), ly_scm2rational (alter));

  return p.smobbed_copy ();
}

LY_DEFINE (ly_pitch_negate, "ly:pitch-negate", 1, 0, 0, (SCM p),
           "Negate @var{p}.")
{
  LY_ASSERT_SMOB (Pitch, p, 1);
  Pitch *pp = unsmob<Pitch> (p);
  return pp->negated ().smobbed_copy ();
}

LY_DEFINE (ly_pitch_steps, "ly:pitch-steps", 1, 0, 0, (SCM p),
           "Number of steps counted from middle@tie{}C of the"
           " pitch@tie{}@var{p}.")
{
  LY_ASSERT_SMOB (Pitch, p, 1);
  Pitch *pp = unsmob<Pitch> (p);
  return scm_from_int (pp->steps ());
}

LY_DEFINE (ly_pitch_octave, "ly:pitch-octave", 1, 0, 0, (SCM pp),
           "Extract the octave from pitch@tie{}@var{pp}.")
{
  LY_ASSERT_SMOB (Pitch, pp, 1);
  Pitch *p = unsmob<Pitch> (pp);
  int q = p->get_octave ();
  return scm_from_int (q);
}

LY_DEFINE (ly_pitch_alteration, "ly:pitch-alteration", 1, 0, 0, (SCM pp),
           "Extract the alteration from pitch@tie{}@var{pp}.")
{
  LY_ASSERT_SMOB (Pitch, pp, 1);
  Pitch *p = unsmob<Pitch> (pp);
  Rational q = p->get_alteration ();

  return ly_rational2scm (q);
}

LY_DEFINE (ly_pitch_notename, "ly:pitch-notename", 1, 0, 0, (SCM pp),
           "Extract the note name from pitch @var{pp}.")
{
  LY_ASSERT_SMOB (Pitch, pp, 1);
  Pitch *p = unsmob<Pitch> (pp);
  int q = p->get_notename ();
  return scm_from_int (q);
}

LY_DEFINE (ly_pitch_tones, "ly:pitch-tones", 1, 0, 0, (SCM pp),
           "Calculate the number of tones of@tie{}@var{pp} from"
           " middle@tie{}C as a rational number.")
{
  LY_ASSERT_SMOB (Pitch, pp, 1);
  return ly_rational2scm (unsmob<Pitch> (pp)->tone_pitch ());
}

LY_DEFINE (ly_pitch_quartertones, "ly:pitch-quartertones", 1, 0, 0, (SCM pp),
           "Calculate the number of quarter tones of@tie{}@var{pp} from"
           " middle@tie{}C.")
{
  LY_ASSERT_SMOB (Pitch, pp, 1);
  Pitch *p = unsmob<Pitch> (pp);
  int q = p->rounded_quartertone_pitch ();
  return scm_from_int (q);
}

LY_DEFINE (ly_pitch_semitones, "ly:pitch-semitones", 1, 0, 0, (SCM pp),
           "Calculate the number of semitones of@tie{}@var{pp} from"
           " middle@tie{}C.")
{
  LY_ASSERT_SMOB (Pitch, pp, 1);
  Pitch *p = unsmob<Pitch> (pp);
  int q = p->rounded_semitone_pitch ();
  return scm_from_int (q);
}

LY_DEFINE (ly_pitch_less_p, "ly:pitch<?", 2, 0, 0, (SCM p1, SCM p2),
           "Is @var{p1} lexicographically smaller than @var{p2}?")
{
  LY_ASSERT_SMOB (Pitch, p1, 1);
  LY_ASSERT_SMOB (Pitch, p2, 2);

  Pitch *a = unsmob<Pitch> (p1);
  Pitch *b = unsmob<Pitch> (p2);

  if (Pitch::compare (*a, *b) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

LY_DEFINE (ly_pitch_diff, "ly:pitch-diff", 2, 0, 0, (SCM pitch, SCM root),
           "Return pitch @var{delta} such that @var{root} transposed by"
           " @var{delta} equals @var{pitch}.")
{

  LY_ASSERT_SMOB (Pitch, pitch, 1);
  LY_ASSERT_SMOB (Pitch, root, 2);

  Pitch *p = unsmob<Pitch> (pitch);
  Pitch *r = unsmob<Pitch> (root);

  return pitch_interval (*r, *p).smobbed_copy ();
}

/* FIXME: probably isn't the right place for this function */
#include "context.hh"
LY_DEFINE (ly_set_middle_C_x, "ly:set-middle-C!", 1, 0, 0, (SCM context),
           "Set the @code{middleCPosition} variable in @var{context}"
           " based on the variables @code{middleCClefPosition} and"
           " @code{middleCOffset}.")
{
  LY_ASSERT_SMOB (Context, context, 1);

  Context *c = unsmob<Context> (context);
  int clef_pos = robust_scm2int (c->get_property ("middleCClefPosition"), 0);
  int offset = robust_scm2int (c->get_property ("middleCOffset"), 0);
  /* middleCCuePosition overrides the clef! */
  SCM cue_pos = c->get_property ("middleCCuePosition");
  if (scm_is_number (cue_pos))
    clef_pos = robust_scm2int (cue_pos, 0);

  c->set_property (ly_symbol2scm ("middleCPosition"),
                   scm_from_int (clef_pos + offset));
  return SCM_UNSPECIFIED;
}
