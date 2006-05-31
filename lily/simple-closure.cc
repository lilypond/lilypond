/*
  closure.cc -- chained closures.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "lily-guile.hh"

static scm_t_bits simple_closure_tag;

bool
is_simple_closure (SCM s)
{
  return (SCM_NIMP (s) && SCM_CELL_TYPE (s) == simple_closure_tag);
}

SCM
simple_closure_expression (SCM smob)
{
  assert (is_simple_closure (smob));
  return (SCM) SCM_CELL_WORD_1(smob);
}

SCM evaluate_with_simple_closure (SCM delayed_argument, SCM expr);

SCM
evaluate_args (SCM delayed_argument, SCM args)
{
  SCM new_args = SCM_EOL;
  SCM *tail = &new_args;
  for (SCM s = args; scm_is_pair (s); s = scm_cdr (s))
    {
      *tail = scm_cons (evaluate_with_simple_closure (delayed_argument, scm_car (s)),
			SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }
  
  return new_args;
}

SCM
evaluate_with_simple_closure (SCM delayed_argument,
			      SCM expr)
{
  if (is_simple_closure (expr))
    {
      SCM inside = simple_closure_expression (expr);
      return scm_apply_1 (scm_car (inside),
			  delayed_argument,
			  evaluate_args (delayed_argument, scm_cdr (inside)));
    }
  else if (!scm_is_pair (expr))
    return expr;
  else if (scm_car (expr) == ly_symbol2scm ("quote"))
    return scm_cadr (expr);
  else if (ly_is_procedure (scm_car (expr)))
    {
      return scm_apply_0 (scm_car (expr),
			  evaluate_args (delayed_argument, scm_cdr (expr)));
    }
  else
    // ugh. deviation from standard. Should print error? 
    return  evaluate_args (delayed_argument, scm_cdr (expr)); 
  
  assert (false);
  return SCM_EOL;
}

LY_DEFINE(ly_simple_closure_p, "ly:simple-closure?",
	  1,0,0, (SCM clos),
	  "Type predicate.")
{
  return scm_from_bool (is_simple_closure (clos));
}

LY_DEFINE(ly_make_simple_closure, "ly:make-simple-closure",
	  1, 0, 0, (SCM expr),
	  "Make a simple closure. @var{expr} should be form of "
	  "@code{(@var{func} @var{a1} @var{A2} ...)}, and will be invoked "
	  "as @code{(@var{func} @var{delayed-arg} @var{a1} @var{a2} ... )}.")
{
  SCM z;

  SCM_NEWSMOB(z, simple_closure_tag, expr);
  return z;
}
 
int
print_simple_closure (SCM s, SCM port, scm_print_state *)
{
  scm_puts ("#<simple-closure ", port);
  scm_display (scm_cdr (s), port);
  scm_puts (" >", port);
  return 1;
}


void init_simple_closure ()
{
  simple_closure_tag = scm_make_smob_type ("simple-closure", 0);
  scm_set_smob_mark (simple_closure_tag, scm_markcdr);
  scm_set_smob_print (simple_closure_tag, print_simple_closure);
};



ADD_SCM_INIT_FUNC(simple_closure, init_simple_closure);
