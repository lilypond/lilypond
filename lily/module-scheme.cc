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

#include "ly-module.hh"
#include "lily-guile.hh"
#include "warn.hh"
#include "std-string.hh"

/*
  If a variable is changed in SRC, then DEST doesn't see the
  definitions.
*/

LY_DEFINE (ly_module_copy, "ly:module-copy", 2, 0, 0, (SCM dest, SCM src),
           R"(
Copy all bindings from module @var{src} into @var{dest}.
           )")
{
#define FUNC_NAME __FUNCTION__
  SCM_VALIDATE_MODULE (1, src);

  auto module_define_closure_func
    = [] (void *closure, SCM key, SCM val, SCM /* result */) {
        SCM module = *static_cast<SCM *> (closure);
        if (from_scm<bool> (scm_variable_bound_p (val)))
          scm_module_define (module, key, scm_variable_ref (val));
        return SCM_EOL;
      };

  ly_scm_hash_fold (module_define_closure_func, static_cast<void *> (&dest),
                    SCM_EOL, SCM_MODULE_OBARRAY (src));
  return SCM_UNSPECIFIED;
}

/* Lookup SYM in a list of modules, which do not have to be related.
   Return the first instance. */
LY_DEFINE (ly_modules_lookup, "ly:modules-lookup", 2, 1, 0,
           (SCM modules, SCM sym, SCM def),
           R"(
Look up @var{sym} in the list @var{modules}, returning the first occurrence.
If not found, return @var{def} or @code{#f} if @var{def} isn't specified.
           )")
{
  for (SCM s = modules; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM mod = scm_car (s);
      SCM v = scm_module_variable (mod, sym);
      if (SCM_VARIABLEP (v) && !SCM_UNBNDP (SCM_VARIABLE_REF (v)))
        return scm_variable_ref (v);
    }

  if (!SCM_UNBNDP (def))
    return def;
  return SCM_BOOL_F;
}
