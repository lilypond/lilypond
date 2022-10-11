/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2008--2022 Han-Wen Nienhuys <hanwen@lilypond.org>


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

#include "prob.hh"

#include "skyline-pair.hh"

LY_DEFINE (ly_paper_system_p, "ly:paper-system?", 1, 0, 0, (SCM obj),
           R"(
Is @var{obj} a C++ @code{Prob} object of type @code{paper-system}?
           )")
{
  return ly_prob_type_p (obj, ly_symbol2scm ("paper-system"));
}

LY_DEFINE (ly_paper_system_minimum_distance, "ly:paper-system-minimum-distance",
           2, 0, 0, (SCM sys1, SCM sys2),
           R"(
Measure the minimum distance between two paper system @code{Prob}s @var{sys1}
and @var{sys2}, using their stored skylines if possible and falling back to
their extents otherwise.
           )")
{
  Real ret = 0;
  Prob *p1 = unsmob<Prob> (sys1);
  Prob *p2 = unsmob<Prob> (sys2);
  SCM sky1_scm = get_property (p1, "vertical-skylines");
  SCM sky2_scm = get_property (p2, "vertical-skylines");
  if (is_scm<Skyline_pair> (sky1_scm) && is_scm<Skyline_pair> (sky2_scm))
    {
      const Skyline_pair &sky1 = from_scm<Skyline_pair> (sky1_scm);
      const Skyline_pair &sky2 = from_scm<Skyline_pair> (sky2_scm);
      ret = sky1[DOWN].distance (sky2[UP]);
    }
  else
    {
      auto *s1 = unsmob<const Stencil> (get_property (p1, "stencil"));
      auto *s2 = unsmob<const Stencil> (get_property (p2, "stencil"));
      Interval iv1 = s1->extent (Y_AXIS);
      Interval iv2 = s2->extent (Y_AXIS);
      ret = iv2[UP] - iv1[DOWN];
    }
  return to_scm (ret);
}
