/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021--2022 Han-Wen Nienhuys

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

#include "protection-pool.hh"

/*
  We have many grobs pointing to each other that it makes sense to
  centralize the marking. This is done by keeping track of all grobs
  in a separate resizable array in the original System grob.

  This data is only accessed during garbage collection, so it is cold
  in CPU cache; using a linked list results in a ~3% slowdown.
*/
SCM
new_protection_pool ()
{
  SCM vec = scm_c_make_vector (128, SCM_BOOL_F);
  return scm_cons (vec, to_scm (0));
}

void
protection_pool_add (SCM pool, SCM obj)
{
  SCM vec = scm_car (pool);
  size_t len = from_scm<size_t> (scm_cdr (pool));
  size_t cap = scm_c_vector_length (scm_car (pool));

  if (len == cap)
    {
      SCM new_vec = scm_c_make_vector (2 * cap, SCM_BOOL_F);
      for (size_t i = 0; i < len; i++)
        scm_c_vector_set_x (new_vec, i, scm_c_vector_ref (vec, i));
      scm_set_car_x (pool, new_vec);
      vec = new_vec;
    }

  scm_c_vector_set_x (vec, len++, obj);
  scm_set_cdr_x (pool, to_scm (len));
}
