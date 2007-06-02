/*
  spring.cc -- implement Spring

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "spring.hh"
#include "warn.hh"
#include "ly-smobs.icc"

Spring::Spring ()
{
  distance_ = 1.0;
  min_distance_ = 1.0;
  inverse_stretch_strength_ = 1.0;
  inverse_compress_strength_ = 1.0;
  other_ = 0;
}

IMPLEMENT_SIMPLE_SMOBS (Spring);

SCM
Spring::mark_smob (SCM x)
{
  (void)x;

  return SCM_UNSPECIFIED;
}

int
Spring::print_smob (SCM, SCM p, scm_print_state *)
{
  scm_puts ("#<Spring smob>", p);
  return 1;
}

SCM
Spring::equal_p (SCM a, SCM b)
{
  return a == b? SCM_BOOL_T : SCM_BOOL_F;
}

