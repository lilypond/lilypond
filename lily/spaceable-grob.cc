/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cstdio>

#include "warn.hh"
#include "spring.hh"
#include "pointer-group-interface.hh"
#include "grob.hh"
#include "paper-column.hh"
#include "international.hh"

SCM
Spaceable_grob::get_minimum_distances (Grob *me)
{
  return me->get_object ("minimum-distances");
}

/*todo: merge code of spring & rod?
 */
void
Spaceable_grob::add_rod (Grob *me, Grob *p, Real d)
{
  //  printf ("rod %lf\n", d);
  if (d < 0)
    return;

  if (isinf (d))
    programming_error ("infinite rod");

  SCM mins = get_minimum_distances (me);
  SCM newdist = scm_from_double (d);
  for (SCM s = mins; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM dist = scm_car (s);
      if (scm_car (dist) == p->self_scm ())
	{
	  scm_set_cdr_x (dist, scm_max (scm_cdr (dist),
					newdist));
	  return;
	}
    }

  if (Paper_column::get_rank (p) < Paper_column::get_rank (me))
    programming_error ("Adding reverse rod");

  mins = scm_cons (scm_cons (p->self_scm (), newdist), mins);
  me->set_object ("minimum-distances", mins);
}

void
Spaceable_grob::add_spring (Grob *me, Grob *other, Spring sp)
{
  SCM ideal = me->get_object ("ideal-distances");

  ideal = scm_cons (scm_cons (sp.smobbed_copy (), other->self_scm ()), ideal);
  me->set_object ("ideal-distances", ideal);
}

Spring
Spaceable_grob::get_spring (Grob *this_col, Grob *next_col)
{
  Spring *spring = 0;

  for (SCM s = this_col->get_object ("ideal-distances");
       !spring && scm_is_pair (s);
       s = scm_cdr (s))
    {
      if (scm_is_pair (scm_car (s))
	  && unsmob_grob (scm_cdar (s)) == next_col
	  && unsmob_spring (scm_caar (s)))
	spring = unsmob_spring (scm_caar (s));
    }

  if (!spring)
    programming_error (_f ("No spring between column %d and next one",
			   Paper_column::get_rank (this_col)));

  return spring ? *spring : Spring ();
}



ADD_INTERFACE (Spaceable_grob,
	       "A layout object that takes part in the spacing problem.",

	       /* properties */
	       "allow-loose-spacing "
	       "ideal-distances "
	       "keep-inside-line "
	       "left-neighbor "
	       "measure-length "
	       "minimum-distances "
	       "right-neighbor "
	       "spacing-wishes "
	       );

