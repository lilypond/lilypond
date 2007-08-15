/*
  spring.cc -- implement Spring

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "spring.hh"
#include "warn.hh"
#include "ly-smobs.icc"

Spring_smob::Spring_smob ()
{
  distance_ = 0.;
  inverse_strength_ = 1.0;
  expand_only_b_ = false;
  other_ = 0;
}

IMPLEMENT_SIMPLE_SMOBS (Spring_smob);

SCM
Spring_smob::mark_smob (SCM x)
{
  (void)x;

  return SCM_UNSPECIFIED;
}

int
Spring_smob::print_smob (SCM, SCM p, scm_print_state *)
{
  scm_puts ("#<Spring smob>", p);
  return 1;
}

SCM
Spring_smob::equal_p (SCM a, SCM b)
{
  return a == b? SCM_BOOL_T : SCM_BOOL_F;
}

