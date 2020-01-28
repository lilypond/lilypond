/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "prob.hh"

LY_DEFINE (ly_prob_set_property_x, "ly:prob-set-property!", 2, 1, 0,
           (SCM obj, SCM sym, SCM value),
           "Set property @var{sym} of @var{obj} to @var{value}.")
{
  LY_ASSERT_SMOB (Prob, obj, 1);
  Prob *ps = unsmob<Prob> (obj);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  ps->set_property (sym, value);
  return SCM_UNSPECIFIED;
}

/*
  Hmm, this is not orthogonal.
*/
LY_DEFINE (ly_prob_property_p, "ly:prob-property?", 2, 1, 0, (SCM obj, SCM sym),
           "Is boolean prop @var{sym} of @var{sym} set?")
{
  return scm_equal_p (SCM_BOOL_T, ly_prob_property (obj, sym, SCM_BOOL_F));
}

LY_DEFINE (ly_prob_property, "ly:prob-property", 2, 1, 0,
           (SCM prob, SCM sym, SCM val),
           "Return the value for property @var{sym} of Prob object"
           " @var{prob}.  If no value is found, return @var{val} or"
           " @code{'()} if @var{val} is not specified.")
{
  LY_ASSERT_SMOB (Prob, prob, 1);
  Prob *ps = unsmob<Prob> (prob);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  if (SCM_UNBNDP (val))
    val = SCM_EOL;

  SCM retval = ps->get_property (sym);
  if (scm_is_null (retval))
    return val;
  else
    return retval;
}

LY_DEFINE (ly_prob_type_p, "ly:prob-type?", 2, 0, 0, (SCM obj, SCM type),
           "Is @var{obj} the specified prob-type?")
{
  Prob *prob = unsmob<Prob> (obj);
  return scm_from_bool (prob && scm_is_eq (prob->type (), type));
}

LY_DEFINE (ly_make_prob, "ly:make-prob", 2, 0, 1,
           (SCM type, SCM init, SCM rest), "Create a @code{Prob} object.")
{
  Prob *pr = new Prob (type, init);

  for (SCM s = rest; scm_is_pair (s) && scm_is_pair (scm_cdr (s));
       s = scm_cddr (s))
    {
      SCM sym = scm_car (s);
      SCM val = scm_cadr (s);

      pr->set_property (sym, val);
    }

  return pr->unprotect ();
}

LY_DEFINE (ly_prob_mutable_properties, "ly:prob-mutable-properties", 1, 0, 0,
           (SCM prob), "Retrieve an alist of mutable properties.")
{
  LY_ASSERT_SMOB (Prob, prob, 1);
  Prob *ps = unsmob<Prob> (prob);
  return ps->get_property_alist (true);
}

LY_DEFINE (ly_prob_immutable_properties, "ly:prob-immutable-properties", 1, 0,
           0, (SCM prob), "Retrieve an alist of immutable properties.")
{
  LY_ASSERT_SMOB (Prob, prob, 1);
  Prob *ps = unsmob<Prob> (prob);
  return ps->get_property_alist (false);
}
