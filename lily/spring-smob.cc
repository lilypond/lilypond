/*   
  spring.cc --  implement Spring
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "spring.hh"
#include "debug.hh"
#include "ly-smobs.icc"

Spring_smob::Spring_smob()
{
  distance_f_ =0.;
  strength_f_ =1.0;
  expand_only_b_ = false;
  other_ = 0;
}


IMPLEMENT_SIMPLE_SMOBS(Spring_smob);

SCM
Spring_smob::mark_smob (SCM) { return SCM_UNSPECIFIED; }

int
Spring_smob::print_smob (SCM s, SCM p, scm_print_state *)
{
  Spring_smob *ss = unsmob_spring (s);
  scm_puts (_f("#<spring smob d= %f>", ss->distance_f_).ch_C(), p);
  return 1;
}

SCM
Spring_smob::equal_p (SCM a , SCM b)
{
  return a==b? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
Spring_smob::smobbed_copy ()const
{
  Spring_smob *  p = new Spring_smob (*this);
  return p->smobbed_self ();
}
