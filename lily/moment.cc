/*   
  moment.cc --  implement Moment
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "lily-guile.hh"
#include "moment.hh"
#include "warn.hh"
#include "ly-smobs.icc"

IMPLEMENT_UNSMOB(Moment,moment);
IMPLEMENT_SIMPLE_SMOBS(Moment);
IMPLEMENT_TYPE_P (Moment, "moment?");

SCM
Moment::mark_smob (SCM)
{
  return SCM_EOL;
}


SCM
Moment::smobbed_copy () const
{
  Moment * m = new Moment (*this);
  return m->smobbed_self();
}


int
Moment::print_smob (SCM s, SCM port, scm_print_state *)
{
  Moment  *r = (Moment *) gh_cdr (s);
     
  scm_puts ("#<Mom ", port);
  String str(r->str());
  scm_puts ((char *)str.ch_C(), port);
  scm_puts (" >", port);
  
  return 1;
}

/*
  TODO: add optional factor argument.
 */
SCM
make_rational (SCM n, SCM d)
{
  Moment m (1,1);

  if (SCM_INUMP (n) && SCM_INUMP(d))
    {
      m =  Moment (gh_scm2int (n), gh_scm2int (d));
    }
  else
    {
      ::error ("make-moment takes two integer arguments. Using 1/1");
    }

  return m.smobbed_copy ();
}


void
init_moments ()
{
  scm_make_gsubr ("make-moment", 2 , 0, 0, (Scheme_function_unknown) make_rational);
}

ADD_SCM_INIT_FUNC(moms,init_moments);

SCM
Moment::equal_p (SCM a, SCM b)
{
  Moment *m1 = unsmob_moment (a);
  Moment *m2 = unsmob_moment (b);
      
  return (*m1 == *m2) ? SCM_BOOL_T : SCM_BOOL_F;
}

