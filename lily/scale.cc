/* 
  scale.cc -- implement Scale
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2006--2009 Han-Wen Nienhuys <hanwen@lilypond.org>
      2007--2008 Rune Zedeler
      2008       Joe Neeman <joeneeman@gmail.com>
*/

#include "scale.hh"

#include "ly-smobs.icc"

/*
  todo: put string <-> pitch here too.

*/
LY_DEFINE (ly_make_scale, "ly:make-scale",
	   1, 0, 0, (SCM steps),
	   "Create a scale."
	   "  The argument is a vector of rational numbers, each of which"
	   " represents the number of tones of a pitch above the tonic.")
{
  bool type_ok = scm_is_vector (steps);

  vector<Rational> tones; 
  if (type_ok)
    {
      int len = scm_c_vector_length (steps);
      for (int i = 0 ; i < len; i++)
	{
 	  SCM step = scm_c_vector_ref (steps, i);
	  type_ok = type_ok && scm_is_rational (step);
	  if (type_ok)
	    {
	      Rational from_c (scm_to_int (scm_numerator (step)),
			       scm_to_int (scm_denominator (step)));
	      tones.push_back (from_c);
	    }
	}
    }
  
  
  SCM_ASSERT_TYPE (type_ok, steps, SCM_ARG1, __FUNCTION__, "vector of rational");

  Scale *s = new Scale (tones);

  SCM retval =  s->self_scm ();
  s->unprotect ();
  
  return retval;
}

LY_DEFINE (ly_default_scale, "ly:default-scale",
	   0, 0, 0, (),
	   "Get the global default scale.")
{
  return default_global_scale
    ? default_global_scale->self_scm ()
    : SCM_BOOL_F;
}


Scale * default_global_scale = 0;

LY_DEFINE (ly_set_default_scale, "ly:set-default-scale",
	   1, 0, 0, (SCM scale),
	   "Set the global default scale.")
{
  LY_ASSERT_SMOB (Scale, scale, 1);

  Scale *s = Scale::unsmob (scale);
  if (default_global_scale)
    default_global_scale->unprotect ();
  default_global_scale = s;
  s->protect ();
  
  return SCM_UNSPECIFIED;
}

int
Scale::step_count () const
{
  return step_tones_.size ();
}

Rational
Scale::tones_at_step (int step, int octave) const
{
  int normalized_step = normalize_step (step);

  octave += (step - normalized_step) / step_count ();

  // There are 6 tones in an octave.
  return step_tones_[normalized_step] + Rational (octave*6);
}

Rational
Scale::step_size (int step) const
{
  int normalized_step = normalize_step (step);

  // Wrap around if we are asked for the final note of the
  // scale (6 is the number of tones of the octave above the
  // first note).
  if (normalized_step + 1 == step_count ())
    return Rational(6) - step_tones_[normalized_step];

  return step_tones_[normalized_step + 1] - step_tones_[normalized_step];
}

int
Scale::normalize_step (int step) const
{
  int ret = step % step_count ();
  if (ret < 0)
    ret += step_count ();

  return ret;
}

int
Scale::print_smob (SCM /* x */,
		   SCM port,
		   scm_print_state *)
{
  scm_puts ("#<Scale>", port); 
  return 1;
}

SCM
Scale::mark_smob (SCM)
{
  return SCM_UNSPECIFIED;
}

Scale::Scale (vector<Rational> const &tones)
{
  step_tones_ = tones;

  smobify_self ();
}

Scale::Scale (Scale const &src)
{
  step_tones_ = src.step_tones_;
  smobify_self ();
}


Scale::~Scale ()
{
}

IMPLEMENT_SMOBS (Scale);
IMPLEMENT_DEFAULT_EQUAL_P (Scale);
