/*
  paper-system-scheme.cc -- implement Prob bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "prob.hh"

LY_DEFINE (ly_prob_set_property_x, "ly:prob-set-property!",
	   2, 1, 0, (SCM system, SCM sym, SCM value),
	   "Set property @var{sym} of @var{system} to @var{value}")
{
  Prob *ps = unsmob_prob (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "Prob");
  SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  ps->internal_set_property (sym, value);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_prob_property, "ly:prob-property",
	   2, 1, 0, (SCM system, SCM sym, SCM dfault),
	   "Return the value for @var{sym}.")
{
  Prob *ps = unsmob_prob (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "Prob");
  SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  if (dfault == SCM_UNDEFINED)
    dfault = SCM_EOL;

  SCM retval = ps->internal_get_property (sym);
  if (retval == SCM_EOL)
    return dfault;
  else
    return retval;
}

LY_DEFINE (ly_prob_type_p, "ly:prob-type?",
	   1, 0, 0,
	   (SCM obj, SCM type),
	   "If obj the specified prob-type?")
{
  Prob*prob = unsmob_prob (obj);
  return scm_from_bool (prob && prob->type() == type);
}

LY_DEFINE (ly_make_prob, "ly:make-prob",
	   2, 0, 0,
	   (SCM type, SCM init),
	   "Create a Prob.")
{
  Prob *pr = new Prob (type, init);
  SCM x = pr->self_scm () ;
  return scm_gc_unprotect_object (x);
}

  
LY_DEFINE(ly_paper_system_p, "ly:paper-system?",
	  1, 0, 0, (SCM obj),
	  "Type predicate.")
{
  return ly_prob_type_p (obj, ly_symbol2scm ("paper-system"));
}
