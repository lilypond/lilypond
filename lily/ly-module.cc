/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "lily-guile.hh"
#include "ly-module.hh"
#include "warn.hh"
#include "protected-scm.hh"
#include "lily-imports.hh"

SCM
ly_make_module (bool safe)
{
  SCM mod = SCM_EOL;
  if (!safe)
    {
      /* Look up (evaluate) Scheme make-module function and call it */
      mod = Guile_user::make_module ();
      Guile_user::module_export_all_x (mod);
      ly_use_module (mod, Guile_user::the_root_module);
      ly_use_module (mod, Lily::module);
    }
  else
    {
      /* Evaluate and call make-safe-lilypond-module */
      mod = Lily::make_safe_lilypond_module ();
    }

  return mod;
}

SCM
ly_use_module (SCM mod, SCM used)
{
  /*
    Pick up the module's interface definition.
    TODO - Replace inline evaluations (interpreted)
    with guile API calls if these become available.
  */
  /*
    Set up to interpret
    '(module_use! <mod> (module-public-interface <used>))'
  */
  return Guile_user::module_use_x (mod,
                                   Guile_user::module_public_interface (used));
}

bool
is_module_internal_symbol (SCM key)
{
  // The public interface of a module is another module stored under
  // the symbol %module-public-interface. Copying its binding about
  // leads to sharing of public interfaces, with confusing
  // consequences. For example, exporting 'sym from one module, will
  // export it from other modules too (with potentially the same
  // value!)
  return scm_is_eq (key, ly_symbol2scm ("%module-public-interface"));
}

#define FUNC_NAME __FUNCTION__

SCM
ly_module_symbols (SCM mod)
{
  SCM_VALIDATE_MODULE (1, mod);

  SCM obarr = SCM_MODULE_OBARRAY (mod);
  SCM syms = ly_hash_table_keys (obarr);
  SCM filtered = SCM_EOL;
  for (SCM s = syms; scm_is_pair (s); s = scm_cdr (s))
    {
      if (!is_module_internal_symbol (scm_car (s)))
        filtered = scm_cons (scm_car (s), filtered);
    }
  return filtered;
}

LY_DEFINE (ly_module_2_alist, "ly:module->alist",
           1, 0, 0, (SCM mod),
           R"(
Dump the contents of module @var{mod} as an alist.
           )")
{
  SCM_VALIDATE_MODULE (1, mod);
  SCM obarr = SCM_MODULE_OBARRAY (mod);

  auto entry_to_alist = [] (void * /* closure */,
                            SCM key,
                            SCM val,
                            SCM result)
  {
    if (from_scm<bool> (scm_variable_bound_p (val)))
      return scm_cons (scm_cons (key, scm_variable_ref (val)), result);
    programming_error ("unbound variable in module");
    return result;
  };

  return ly_scm_hash_fold (entry_to_alist, nullptr, SCM_EOL, obarr);
}

void
ly_export (SCM module, SCM namelist)
{
  Guile_user::module_export_x (module, namelist);
}

void
ly_reexport_module (SCM mod)
{
  ly_export (mod, ly_module_symbols (mod));
}
