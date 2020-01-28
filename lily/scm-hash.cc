/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  return Smob1::make_smob (scm_c_make_hash_table (119));
}

int
Scheme_hash_table::print_smob (SCM p, scm_print_state *) const
{
  scm_puts ("#<Scheme_hash_table  ", p);
  scm_display (hash_tab (), p);
  scm_puts ("> ", p);
  return 1;
}

bool
Scheme_hash_table::try_retrieve (SCM k, SCM *v)
{

  SCM handle = scm_hashq_get_handle (hash_tab (), k);
  if (scm_is_pair (handle))
    {
      *v = scm_cdr (handle);
      return true;
    }
  else
    return false;
}

bool
Scheme_hash_table::contains (SCM k) const
{
  return scm_is_pair (scm_hashq_get_handle (hash_tab (), k));
}

void
Scheme_hash_table::set (SCM k, SCM v)
{
  assert (scm_is_symbol (k));
  SCM handle = scm_hashq_create_handle_x (hash_tab (), k, SCM_UNDEFINED);
  scm_set_cdr_x (handle, v);
}

SCM
Scheme_hash_table::get (SCM k) const
{
  /* SCM_UNDEFINED is the default for unset elements, but
     scm_hashq_ref cannot return it, so we do it a bit more awkwardly.
  */
  SCM handle = scm_hashq_get_handle (hash_tab (), k);
  if (scm_is_pair (handle))
    return scm_cdr (handle);
  return SCM_UNDEFINED;
}

void
Scheme_hash_table::remove (SCM k)
{
  scm_hashq_remove_x (hash_tab (), k);
}

static SCM
collect_handles (void * /* closure */, SCM key, SCM value, SCM result)
{
  return scm_acons (key, value, result);
}

SCM
Scheme_hash_table::to_alist () const
{
  return scm_internal_hash_fold ((scm_t_hash_fold_fn)&collect_handles, NULL,
                                 SCM_EOL, hash_tab ());
}
