/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "scm-hash.hh"

#include <cassert>

SCM
Scheme_hash_table::make_smob ()
{
  return scm_c_make_hash_table (119);
}

void
Scheme_hash_table::set (SCM k, SCM v)
{
  SCM handle = scm_hashq_create_handle_x (hash_tab (), k, SCM_UNDEFINED);
  scm_set_cdr_x (handle, v);
}

std::optional<SCM>
Scheme_hash_table::get (SCM k) const
{
  auto result = std::optional<SCM> {};
  SCM handle = scm_hashq_get_handle (hash_tab (), k);
  if (scm_is_pair (handle))
    result = scm_cdr (handle);
  return result;
}

void
Scheme_hash_table::remove (SCM k)
{
  scm_hashq_remove_x (hash_tab (), k);
}

SCM
Scheme_hash_table::to_alist () const
{
  auto collect_handles
    = [] (void * /* closure */, SCM key, SCM value, SCM result) {
        return scm_acons (key, value, result);
      };

  return ly_scm_hash_fold (collect_handles, nullptr, SCM_EOL, hash_tab ());
}
