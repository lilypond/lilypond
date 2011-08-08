/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2011 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "warn.hh"
#include "main.hh"
#include "std-string.hh"
#include "protected-scm.hh"

SCM
ly_make_module (bool safe)
{
  SCM mod = SCM_EOL;
  if (!safe)
    {
      /* Look up (evaluate) Scheme make-module function and call it */

      SCM maker = ly_lily_module_constant ("make-module");
      mod = scm_call_0 (maker);
      /*
        Look up and call Guile module-export-all! or, when using
        Guile V1.8, the compatible shim defined in lily.scm.
      */
      SCM module_export_all_x = ly_lily_module_constant ("module-export-all!");
      scm_call_1 (module_export_all_x, mod);

      /*
        Evaluate Guile module "the-root-module",
        and ensure we inherit definitions from it and the "lily" module
        N.B. this used to be "the-scm-module" and is deprecated in
        Guile V1.9/2.0
      */
      SCM scm_module = ly_lily_module_constant ("the-root-module");
      ly_use_module (mod, scm_module);
      ly_use_module (mod, global_lily_module);
    }
  else
    {
      /* Evaluate and call make-safe-lilypond-module */
      SCM proc = ly_lily_module_constant ("make-safe-lilypond-module");
      mod = scm_call_0 (proc);
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
  SCM scm_module_use = ly_symbol2scm ("module-use!");
  SCM scm_module_public_interface = ly_symbol2scm ("module-public-interface");
  SCM iface = scm_list_2 (scm_module_public_interface, used);
  /*
    Set up to interpret
    '(module_use! <mod> (module-public-interface <used>))'
  */
  SCM expr = scm_list_3 (scm_module_use, mod, iface);
  /*
    Now return SCM value, this is the result of interpreting
    '(eval (module-use! <mod> (module-public-interface <used>)) "lily")'
  */
  return scm_eval (expr, global_lily_module);
}

#define FUNC_NAME __FUNCTION__

SCM
ly_module_symbols (SCM mod)
{
  SCM_VALIDATE_MODULE (1, mod);

  SCM obarr = SCM_MODULE_OBARRAY (mod);
  return ly_hash_table_keys (obarr);
}

static SCM
entry_to_alist (void * /* closure */,
                SCM key,
                SCM val,
                SCM result)
{
  if (scm_variable_bound_p (val) == SCM_BOOL_T)
    return scm_cons (scm_cons (key, scm_variable_ref (val)), result);
  programming_error ("unbound variable in module");
  return result;
}

LY_DEFINE (ly_module_2_alist, "ly:module->alist",
           1, 0, 0, (SCM mod),
           "Dump the contents of module @var{mod} as an alist.")
{
  SCM_VALIDATE_MODULE (1, mod);
  SCM obarr = SCM_MODULE_OBARRAY (mod);

  return scm_internal_hash_fold ((scm_t_hash_fold_fn) &entry_to_alist,
                                 NULL, SCM_EOL, obarr);
}

void
ly_export (SCM module, SCM namelist)
{
  static SCM export_function;
  if (!export_function)
    export_function = scm_permanent_object (scm_c_lookup ("module-export!"));

  scm_call_2 (SCM_VARIABLE_REF (export_function), module, namelist);
}

void
ly_reexport_module (SCM mod)
{
  ly_export (mod, ly_module_symbols (mod));
}
