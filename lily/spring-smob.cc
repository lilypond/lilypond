/*
  spring.cc -- implement Spring

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "spring.hh"
#include "warn.hh"
#include "ly-smobs.icc"

IMPLEMENT_SIMPLE_SMOBS (Spring);

SCM
Spring::mark_smob (SCM)
{
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

