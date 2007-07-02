/* 
  scale.cc -- implement Scale
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2006--2007 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "scale.hh"

#include "ly-smobs.icc"

/*
  todo: put string <-> pitch here too.

*/
LY_DEFINE (ly_make_scale, "ly:make-scale",
	   1, 0, 0, (SCM steps),
	   "Create a scale.  Takes a vector of integers as argument.")
{
  bool type_ok = scm_is_vector (steps);

  vector<Rational> semitones; 
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
	      semitones.push_back (from_c);
	    }
	}
    }
  
  SCM_ASSERT_TYPE (type_ok, steps, SCM_ARG1, __FUNCTION__, "vector of int");

  Scale *s = new Scale;
  s->step_tones_ = semitones;

  SCM retval =  s->self_scm ();

  s->unprotect ();
  
  return retval;
}

LY_DEFINE (ly_default_scale, "ly:default-scale",
	   0, 0, 0, (),
	   "Get the global default scale.")
{
  return default_global_scale
    ? SCM_BOOL_F
    : default_global_scale->self_scm ();
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
Scale::print_smob (SCM x, SCM port, scm_print_state *)
{
  (void) x;
  
  scm_puts ("#<Scale>", port); 
  return 1;
}


SCM
Scale::mark_smob (SCM x)
{
  (void) x;
  return SCM_UNSPECIFIED;
}

Scale::Scale ()
{
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
