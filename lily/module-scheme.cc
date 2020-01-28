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

#include "ly-module.hh" // pulls in lily-guile.hh and guile-compatibility.hh
#include "main.hh"
#include "std-string.hh"
#include "warn.hh"

/*
  If a variable is changed in SRC, then DEST doesn't see the
  definitions.
*/

static SCM
module_define_closure_func (void *closure, SCM key, SCM val, SCM /* result */)
{
  SCM module = *static_cast<SCM *> (closure);
  if (to_boolean (scm_variable_bound_p (val)))
    scm_module_define (module, key, scm_variable_ref (val));
  return SCM_EOL;
}

LY_DEFINE (ly_module_copy, "ly:module-copy", 2, 0, 0, (SCM dest, SCM src),
           "Copy all bindings from module @var{src} into @var{dest}.")
{
#define FUNC_NAME __FUNCTION__
  SCM_VALIDATE_MODULE (1, src);
  scm_internal_hash_fold ((scm_t_hash_fold_fn)&module_define_closure_func,
                          static_cast<void *> (&dest), SCM_EOL,
                          SCM_MODULE_OBARRAY (src));
  return SCM_UNSPECIFIED;
}

/* Lookup SYM, but don't give error when it is not defined.
   N.B. this is only needed when running with Guile versions
   prior to V2.0.3, when calls to ly_module_lookup can be replaced
   with direct calls to the Guile API scm_module_variable in the
   LilyPond codebase.
*/
SCM
ly_module_lookup (SCM module, SCM sym)
{
#define FUNC_NAME __FUNCTION__
  SCM_VALIDATE_MODULE (1, module);
/*
  Issue 2758:
    Guile V2 onward has a scm_module_variable API module.
    Guile V1.8.7 only has a (module-variable) REPL function and we
    can't import this via Scm_variable since that needs
    ly_module_lookup itself.
 */
#if GUILEV1
  return scm_sym2var (sym, scm_module_lookup_closure (module), SCM_BOOL_F);
#else
  return scm_module_variable (module, sym);
#endif
#undef FUNC_NAME
}

/* Lookup SYM in a list of modules, which do not have to be related.
   Return the first instance. */
LY_DEFINE (ly_modules_lookup, "ly:modules-lookup", 2, 1, 0,
           (SCM modules, SCM sym, SCM def),
           "Look up @var{sym} in the list @var{modules},"
           " returning the first occurence.  If not found, return"
           " @var{def} or @code{#f} if @var{def} isn't specified.")
{
  for (SCM s = modules; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM mod = scm_car (s);
      SCM v = ly_module_lookup (mod, sym);
      if (SCM_VARIABLEP (v) && !SCM_UNBNDP (SCM_VARIABLE_REF (v)))
        return scm_variable_ref (v);
    }

  if (!SCM_UNBNDP (def))
    return def;
  return SCM_BOOL_F;
}
