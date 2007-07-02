/*
  paper-system-scheme.cc -- implement Prob bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "prob.hh"
#include "skyline.hh"

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

  
LY_DEFINE (ly_paper_system_p, "ly:paper-system?",
	  1, 0, 0, (SCM obj),
	  "Type predicate.")
{
  return ly_prob_type_p (obj, ly_symbol2scm ("paper-system"));
}

LY_DEFINE (ly_paper_system_minimum_distance, "ly:paper-system-minimum-distance",
	   2, 0, 0, (SCM sys1, SCM sys2),
	   "Measure the minimum distance between these two paper-systems,"
	   " using their stored skylines if possible and falling back to"
	   " their extents otherwise.")
{
  Real ret = 0;
  Prob *p1 = unsmob_prob (sys1);
  Prob *p2 = unsmob_prob (sys2);
  Skyline_pair *sky1 = Skyline_pair::unsmob (p1->get_property ("vertical-skylines"));
  Skyline_pair *sky2 = Skyline_pair::unsmob (p2->get_property ("vertical-skylines"));

  if (sky1 && sky2)
    ret = (*sky1)[DOWN].distance ((*sky2)[UP]);
  else
    {
      Stencil *s1 = unsmob_stencil (p1->get_property ("stencil"));
      Stencil *s2 = unsmob_stencil (p2->get_property ("stencil"));
      Interval iv1 = s1->extent (Y_AXIS);
      Interval iv2 = s2->extent (Y_AXIS);
      ret = iv2[UP] - iv1[DOWN];
    }
  return scm_from_double (ret);
}
