/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "spaceable-grob.hh"

#include "warn.hh"
#include "spring.hh"
#include "pointer-group-interface.hh"
#include "grob.hh"
#include "paper-column.hh"
#include "international.hh"

#include <cstdio>

SCM
Spaceable_grob::get_minimum_distances (Grob *me)
{
  return get_object (me, "minimum-distances");
}

/*todo: merge code of spring & rod?
 */
void
Spaceable_grob::add_rod (Paper_column *me, Paper_column *p, Real d)
{
  //  printf ("rod %lf\n", d);
  if (d < 0)
    return;

  if (std::isinf (d))
    programming_error ("infinite rod");

  SCM mins = get_minimum_distances (me);
  SCM newdist = to_scm (d);
  for (SCM s = mins; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM dist = scm_car (s);
      if (scm_is_eq (scm_car (dist), p->self_scm ()))
        {
          scm_set_cdr_x (dist, scm_max (scm_cdr (dist), newdist));
          return;
        }
    }

  if (p->get_rank () < me->get_rank ())
    programming_error ("Adding reverse rod");

  mins = scm_cons (scm_cons (p->self_scm (), newdist), mins);
  set_object (me, "minimum-distances", mins);
}

void
Spaceable_grob::add_spring (Grob *me, Grob *other, Spring const &sp)
{
  SCM ideal = get_object (me, "ideal-distances");

  ideal = scm_cons (scm_cons (sp.smobbed_copy (), other->self_scm ()), ideal);
  set_object (me, "ideal-distances", ideal);
}

Spring
Spaceable_grob::get_spring (Paper_column *this_col, Grob *next_col)
{
  for (SCM s = get_object (this_col, "ideal-distances"); scm_is_pair (s);
       s = scm_cdr (s))
    {
      if (scm_is_pair (scm_car (s)) && unsmob<Grob> (scm_cdar (s)) == next_col)
        {
          if (Spring *spring = unsmob<Spring> (scm_caar (s)))
            return *spring;
        }
    }

  programming_error (to_string ("No spring between column %d and next one",
                                this_col->get_rank ()));
  return {};
}

ADD_INTERFACE (Spaceable_grob,
               R"(
A layout object that takes part in the spacing problem.
               )",

               /* properties */
               R"(
allow-loose-spacing
ideal-distances
keep-inside-line
left-neighbor
measure-length
minimum-distances
right-neighbor
spacing-wishes
               )");
