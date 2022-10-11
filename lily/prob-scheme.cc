/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
           R"(
Set property @var{sym} of @var{obj} to @var{value}.
           )")
{
  auto *const ps = LY_ASSERT_SMOB (Prob, obj, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  set_property (ps, sym, value);
  return SCM_UNSPECIFIED;
}

/*
  Hmm, this is not orthogonal.
*/
LY_DEFINE (ly_prob_property_p, "ly:prob-property?", 2, 0, 0, (SCM obj, SCM sym),
           R"(
Is boolean prop @var{sym} of @var{obj} set?
           )")
{
  return scm_equal_p (SCM_BOOL_T, ly_prob_property (obj, sym, SCM_BOOL_F));
}

LY_DEFINE_WITH_SETTER (ly_prob_property, "ly:prob-property",
                       ly_prob_set_property_x, 2, 1, 0,
                       (SCM prob, SCM sym, SCM val),
                       R"(
Return the value for property @var{sym} of Prob object @var{prob}.  If no value
is found, return @var{val} or @code{'()} if @var{val} is not specified.
                       )")
{
  auto *const ps = LY_ASSERT_SMOB (Prob, prob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  if (SCM_UNBNDP (val))
    val = SCM_EOL;

  SCM retval = get_property (ps, sym);
  if (scm_is_null (retval))
    return val;
  else
    return retval;
}

LY_DEFINE (ly_prob_type_p, "ly:prob-type?", 2, 0, 0, (SCM obj, SCM type),
           R"(
Is @var{obj} the specified prob type?
           )")
{
  auto *prob = unsmob<Prob> (obj);
  return to_scm (prob && scm_is_eq (prob->type (), type));
}

LY_DEFINE (ly_make_prob, "ly:make-prob", 2, 0, 1,
           (SCM type, SCM init, SCM rest),
           R"(
Create a @code{Prob} object.
           )")
{
  Prob *pr = new Prob (type, init);

  for (SCM s = rest; scm_is_pair (s) && scm_is_pair (scm_cdr (s));
       s = scm_cddr (s))
    {
      SCM sym = scm_car (s);
      SCM val = scm_cadr (s);

      set_property (pr, sym, val);
    }

  return pr->unprotect ();
}

LY_DEFINE (ly_prob_mutable_properties, "ly:prob-mutable-properties", 1, 0, 0,
           (SCM prob),
           R"(
Retrieve an alist of mutable properties.
           )")
{
  auto *const ps = LY_ASSERT_SMOB (Prob, prob, 1);
  return ps->get_property_alist (true);
}

LY_DEFINE (ly_prob_immutable_properties, "ly:prob-immutable-properties", 1, 0,
           0, (SCM prob),
           R"(
Retrieve an alist of immutable properties.
           )")
{
  auto *const ps = LY_ASSERT_SMOB (Prob, prob, 1);
  return ps->get_property_alist (false);
}
