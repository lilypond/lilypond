/*
  paper-system-scheme.cc -- implement Prob bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "prob.hh"

LY_DEFINE (ly_prob_set_property_x, "ly:prob-set-property!",
	   2, 1, 0, (SCM obj, SCM sym, SCM value),
	   "Set property @var{sym} of @var{obj} to @var{value}.")
{
  LY_ASSERT_SMOB (Prob, obj, 1);
  Prob *ps = unsmob_prob (obj);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  ps->set_property (sym, value);
  return SCM_UNSPECIFIED;
}

/*
  Hmm, this is not orthogonal.
*/
LY_DEFINE (ly_prob_property_p, "ly:prob-property?",
	   2, 1, 0, (SCM obj, SCM sym),
	   "Is boolean prop @var{sym} set?")
{
  return scm_equal_p (SCM_BOOL_T, ly_prob_property (obj, sym, SCM_BOOL_F));
}

LY_DEFINE (ly_prob_property, "ly:prob-property",
	   2, 1, 0, (SCM obj, SCM sym, SCM dfault),
	   "Return the value for @var{sym}.")
{
  LY_ASSERT_SMOB (Prob, obj, 1);
  Prob *ps = unsmob_prob (obj);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  if (dfault == SCM_UNDEFINED)
    dfault = SCM_EOL;

  SCM retval = ps->internal_get_property (sym);
  if (retval == SCM_EOL)
    return dfault;
  else
    return retval;
}

LY_DEFINE (ly_prob_type_p, "ly:prob-type?",
	   2, 0, 0,
	   (SCM obj, SCM type),
	   "Is @var{obj} the specified prob-type?")
{
  Prob*prob = unsmob_prob (obj);
  return scm_from_bool (prob && prob->type () == type);
}

LY_DEFINE (ly_make_prob, "ly:make-prob",
	   2, 0, 1,
	   (SCM type, SCM init, SCM rest),
	   "Create a @code{Prob} object.")
{
  Prob *pr = new Prob (type, init);

  for (SCM s = rest;
       scm_is_pair (s) && scm_is_pair (scm_cdr (s)); s = scm_cddr (s))
    {
      SCM sym = scm_car (s);
      SCM val = scm_cadr (s);

      pr->set_property (sym, val);
    }
  
  return pr->unprotect ();
}


LY_DEFINE (ly_prob_mutable_properties, "ly:prob-mutable-properties",
	   1, 0, 0,
	   (SCM prob),
	   "Retrieve an alist of mutable properties.")
{
  LY_ASSERT_SMOB (Prob, prob, 1);
  Prob *ps = unsmob_prob (prob);
  return ps->get_property_alist (true);
}

LY_DEFINE (ly_prob_immutable_properties, "ly:prob-immutable-properties",
	   1, 0, 0,
	   (SCM prob),
	   "Retrieve an alist of mutable properties.")
{
  LY_ASSERT_SMOB (Prob, prob, 1);
  Prob *ps = unsmob_prob (prob);
  return ps->get_property_alist (false);
}

