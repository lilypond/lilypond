/*   
  moment.cc --  implement Moment
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "lily-guile.hh"
#include "moment.hh"
#include "warn.hh"

SCM
Moment::mark_smob (SCM s)
{
  return SCM_EOL;
}


Moment::~Moment()
{
  self_scm_ = SCM_EOL;
}

int
Moment::print_smob (SCM s, SCM port, scm_print_state *)
{
  Moment  *r = (Moment *) SCM_CDR (s);
     
  scm_puts ("#<Mom ", port);
  String str(r->str());
  scm_puts ((char *)str.ch_C(), port);
  scm_puts (" >", port);
  
  return 1;
}

void
Moment::do_smobify_self ()
{}

SCM
make_rational (SCM n, SCM d)
{
  if (SCM_INUMP (n) && SCM_INUMP(d))
    {
      Moment *r = new Moment (gh_scm2int (n), gh_scm2int (d));
      return r->smobify_self ();
    }
  else
    {
      ::error ("Not a number");
      assert(false);
    }
}

#include "ly-smobs.icc"

IMPLEMENT_SMOBS(Moment);

void
init_moments ()
{
  scm_make_gsubr ("make-moment", 2 , 0, 0,  (SCM(*)(...)) make_rational);
}

ADD_SCM_INIT_FUNC(moms,init_moments);

SCM
Moment::equal_p (SCM a, SCM b)
{
  Moment *m1 = SMOB_TO_TYPE(Moment, a);
  Moment *m2 = SMOB_TO_TYPE(Moment, b);
      
  return (*m1 == *m2) ? SCM_BOOL_T : SCM_BOOL_F;
}
