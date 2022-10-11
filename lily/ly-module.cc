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
ly_make_module ()
{
  SCM mod = Guile_user::make_module ();
  Guile_user::module_export_all_x (mod);
  ly_use_module (mod, Guile_user::the_root_module);
  ly_use_module (mod, Lily::module);
  return mod;
}

SCM
ly_use_module (SCM mod, SCM used)
{
  return Guile_user::module_use_x (mod,
                                   Guile_user::module_public_interface (used));
}

#define FUNC_NAME __FUNCTION__

SCM
ly_module_symbols (SCM mod)
{
  SCM_VALIDATE_MODULE (1, mod);

  SCM obarr = SCM_MODULE_OBARRAY (mod);
  return ly_hash_table_keys (obarr);
}

LY_DEFINE (ly_module_2_alist, "ly:module->alist", 1, 0, 0, (SCM mod),
           R"(
Dump the contents of module @var{mod} as an alist.
           )")
{
  SCM_VALIDATE_MODULE (1, mod);
  SCM obarr = SCM_MODULE_OBARRAY (mod);

  auto entry_to_alist
    = [] (void * /* closure */, SCM key, SCM val, SCM result) {
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
