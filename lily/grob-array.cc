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

#include "grob-array.hh"
#include "grob.hh"
#include "international.hh"

Grob_array::Grob_array ()
{
  ordered_ = true;
}

SCM
Grob_array::mark_smob () const
{
  /* grobs[0]->protection_pool_ should be hotter in cache, but returning it here
     does not make a dent in benchmarks */
  return grobs_.empty () ? SCM_UNDEFINED : grobs_[0]->self_scm ();
}

int
Grob_array::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<Grob_array", port);
  for (vsize i = 0; i < size (); i++)
    {
      scm_display (grob (i)->self_scm (), port);
      scm_puts (" ", port);
    }
  scm_puts (">", port);
  return 1;
}

SCM
Grob_array::make_array ()
{
  Grob_array ga;
  return ga.smobbed_copy ();
}

void
Grob_array::remove_duplicates ()
{
  assert (!ordered_);

  uniquify (grobs_);
}

void
Grob_array::filter (bool (*predicate) (const Grob *))
{
  vsize new_size = 0;
  for (vsize i = 0; i < grobs_.size (); ++i)
    if (predicate (grobs_[i]))
      grobs_[new_size++] = grobs_[i];
  grobs_.resize (new_size);
  grobs_.shrink_to_fit ();
}

const char *const Grob_array::type_p_name_ = "ly:grob-array?";

SCM
grob_list_to_grob_array (SCM lst)
{
  SCM arr_scm = Grob_array::make_array ();
  Grob_array *ga = unsmob<Grob_array> (arr_scm);
  SCM s = lst;
  for (; scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *g = unsmob<Grob> (scm_car (s));
      if (!g)
        warning (_f ("ly:grob-list->grob-array encountered a non-grob object"));
      else
        ga->add (g);
    }
  if (!scm_is_null (s))
    warning (_f ("ly:grob-list->grob-array expected a list"));
  return arr_scm;
}

SCM
grob_array_to_list (Grob_array *array)
{
  SCM list = SCM_EOL;
  SCM *tail = &list;

  for (vsize i = 0; i < array->size (); i++)
    {
      *tail = scm_cons (array->grob (i)->self_scm (), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }
  return list;
}
