/*
  pitch-scheme.cc -- implement scheme functions for Pitch

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "pitch.hh"

LY_DEFINE (ly_pitch_transpose, "ly:pitch-transpose",
	   2, 0, 0, (SCM p, SCM delta),
	   "Transpose @var{p} by the amount @var{delta}, "
	   "where @var{delta} is relative to middle C.")
{
  Pitch *t = unsmob_pitch (p);
  Pitch *d = unsmob_pitch (delta);
  SCM_ASSERT_TYPE (t, p, SCM_ARG1, __FUNCTION__, "pitch");
  SCM_ASSERT_TYPE (d, delta, SCM_ARG1, __FUNCTION__, "pitch");
  return t->transposed (*d).smobbed_copy ();
}

/* Should add optional args.  */
LY_DEFINE (ly_make_pitch, "ly:make-pitch",
	   3, 0, 0, (SCM octave, SCM note, SCM alter),
	   "@var{octave} is specified by an integer, "
	   "zero for the octave containing middle C.  "
	   "@var{note} is a number from 0 to 6, "
	   "with 0 corresponding to C and 6 corresponding to B.  "
	   "The @var{alter} is zero for a natural, negative for "
	   "flats, or positive for sharps. ")
{
  SCM_ASSERT_TYPE (scm_integer_p (octave) == SCM_BOOL_T, octave, SCM_ARG1, __FUNCTION__, "integer");
  SCM_ASSERT_TYPE (scm_integer_p (note) == SCM_BOOL_T, note, SCM_ARG2, __FUNCTION__, "integer");
  SCM_ASSERT_TYPE (scm_integer_p (alter) == SCM_BOOL_T, alter, SCM_ARG3, __FUNCTION__, "integer");

  Pitch p (scm_to_int (octave), scm_to_int (note), scm_to_int (alter));
  return p.smobbed_copy ();
}

LY_DEFINE (ly_pitch_steps, "ly:pitch-steps", 1, 0, 0,
	   (SCM p),
	   "Number of steps counted from middle C of the pitch @var{p}.")
{
  Pitch *pp = unsmob_pitch (p);
  SCM_ASSERT_TYPE (pp, p, SCM_ARG1, __FUNCTION__, "Pitch");
  return scm_from_int (pp->steps ());
}

LY_DEFINE (ly_pitch_octave, "ly:pitch-octave",
	   1, 0, 0, (SCM pp),
	   "Extract the octave from pitch @var{p}.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE (p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q = p->get_octave ();
  return scm_from_int (q);
}

LY_DEFINE (ly_pitch_alteration, "ly:pitch-alteration",
	   1, 0, 0, (SCM pp),
	   "Extract the alteration from pitch  @var{p}.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE (p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q = p->get_alteration ();

  return scm_from_int (q);
}

LY_DEFINE (pitch_notename, "ly:pitch-notename",
	   1, 0, 0, (SCM pp),
	   "Extract the note name from pitch  @var{pp}.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE (p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q = p->get_notename ();
  return scm_from_int (q);
}

LY_DEFINE (ly_pitch_quartertones, "ly:pitch-quartertones",
	   1, 0, 0, (SCM pp),
	   "Calculate the number of quarter tones of @var{p} from middle C.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE (p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q = p->quartertone_pitch ();
  return scm_from_int (q);
}

LY_DEFINE (ly_pitch_semitones, "ly:pitch-semitones",
	   1, 0, 0, (SCM pp),
	   "calculate the number of semitones of @var{p} from middle C.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE (p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q = p->semitone_pitch ();
  return scm_from_int (q);
}

LY_DEFINE (ly_pitch_less_p, "ly:pitch<?",
	   2, 0, 0, (SCM p1, SCM p2),
	   "Is @var{p1} lexicographically smaller than @var{p2}?")
{
  Pitch *a = unsmob_pitch (p1);
  Pitch *b = unsmob_pitch (p2);

  SCM_ASSERT_TYPE (a, p1, SCM_ARG1, __FUNCTION__, "Pitch");
  SCM_ASSERT_TYPE (b, p2, SCM_ARG2, __FUNCTION__, "Pitch");

  if (Pitch::compare (*a, *b) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

LY_DEFINE (ly_pitch_diff, "ly:pitch-diff",
	   2, 0, 0, (SCM pitch, SCM root),
	   "Return pitch @var{delta} such that @code{pitch} transposed by "
	   "@var{delta} equals @var{root}")
{
  Pitch *p = unsmob_pitch (pitch);
  Pitch *r = unsmob_pitch (root);
  SCM_ASSERT_TYPE (p, pitch, SCM_ARG1, __FUNCTION__, "Pitch");
  SCM_ASSERT_TYPE (r, root, SCM_ARG2, __FUNCTION__, "Pitch");

  return pitch_interval (*r, *p).smobbed_copy ();
}
