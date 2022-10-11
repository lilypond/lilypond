/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2022 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "context-mod.hh"

LY_DEFINE (ly_get_context_mods, "ly:get-context-mods", 1, 0, 0,
           (SCM contextmod),
           R"(
Returns the list of context modifications stored in @var{contextmod}.
           )")
{
  auto *const tr = LY_ASSERT_SMOB (Context_mod, contextmod, 1);
  return tr->get_mods ();
}

LY_DEFINE (ly_add_context_mod, "ly:add-context-mod", 2, 0, 0,
           (SCM contextmods, SCM modification),
           R"(
Adds the given context @var{modification} to the list @var{contextmods} of
context modifications.
           )")
{
  auto *const ctxmod = LY_ASSERT_SMOB (Context_mod, contextmods, 1);
  ctxmod->add_context_mod (modification);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_make_context_mod, "ly:make-context-mod", 0, 1, 0, (SCM mod_list),
           R"(
Create a context modification, optionally initialized via the list of
modifications @var{mod-list}.
           )")
{
  if (!SCM_UNBNDP (mod_list))
    {
      LY_ASSERT_TYPE (ly_cheap_is_list, mod_list, 1);
      return Context_mod (mod_list).smobbed_copy ();
    }
  else
    return Context_mod ().smobbed_copy ();
}

LY_DEFINE (ly_context_mod_apply_x, "ly:context-mod-apply!", 2, 0, 0,
           (SCM context, SCM mod),
           R"(
Apply the context modification @var{mod} to @var{context}.
           )")
{
  auto *const c = LY_ASSERT_SMOB (Context, context, 1);
  auto *const cm = LY_ASSERT_SMOB (Context_mod, mod, 2);

  apply_property_operations (c, cm->get_mods ());
  scm_remember_upto_here_1 (context);
  return SCM_UNSPECIFIED;
}
