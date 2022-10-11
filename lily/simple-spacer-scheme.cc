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

#include "paper-column.hh"
#include "spring.hh"
#include "warn.hh"
#include "simple-spacer.hh"

#include <cstdio>

using std::vector;

LY_DEFINE (ly_solve_spring_rod_problem, "ly:solve-spring-rod-problem", 4, 0, 0,
           (SCM springs, SCM rods, SCM length, SCM ragged),
           R"(
Solve a spring and rod problem for @var{count} objects that are connected by
@var{count}-1 @var{springs}, and an arbitrary number of @var{rods}.
@var{count} is implicitly given by @var{springs} and @var{rods}.  The
@var{springs} argument has the format @code{(ideal, inverse_hook)} and
@var{rods} is of the form @code{(idx1, idx2, distance)}.

@var{length} is a number, @var{ragged} a boolean.

The function returns a list containing the force (positive for stretching,
negative for compressing and @code{#f} for non-satisfied constraints) followed
by @var{spring-count}+1 positions of the objects.
           )")
{
  long len = scm_ilength (springs);
  if (len == 0)
    return ly_list (to_scm (0.0), to_scm (0.0));

  SCM_ASSERT_TYPE (len >= 0, springs, SCM_ARG1, __FUNCTION__,
                   "list of springs");
  SCM_ASSERT_TYPE (scm_ilength (rods) > 0, rods, SCM_ARG1, __FUNCTION__,
                   "list of rods");
  LY_ASSERT_TYPE (scm_is_number, length, 3);

  bool is_ragged = from_scm<bool> (ragged);
  Simple_spacer spacer;
  for (SCM s = springs; scm_is_pair (s); s = scm_cdr (s))
    {
      Real ideal = from_scm<double> (scm_caar (s));
      Real inv_hooke = from_scm<double> (scm_cadar (s));

      Spring sp (ideal, 0.0);
      sp.set_inverse_compress_strength (inv_hooke);
      sp.set_inverse_stretch_strength (inv_hooke);

      spacer.add_spring (sp);
    }

  for (SCM s = rods; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      int l = from_scm<int> (scm_car (entry));
      int r = from_scm<int> (scm_cadr (entry));
      entry = scm_cddr (entry);

      Real distance = from_scm<double> (scm_car (entry));
      spacer.add_rod (l, r, distance);
    }

  Simple_spacer::Solution sol
    = spacer.solve (from_scm<double> (length), is_ragged);

  vector<Real> posns = spacer.spring_positions (sol.force_, is_ragged);

  SCM force_return = sol.fits_ ? to_scm (sol.force_) : SCM_BOOL_F;

  SCM retval = SCM_EOL;
  for (vsize i = posns.size (); i--;)
    retval = scm_cons (to_scm (posns[i]), retval);

  retval = scm_cons (force_return, retval);
  return retval;
}
