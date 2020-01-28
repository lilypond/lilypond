/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2020 Joe Neeman <joeneeman@gmail.com>

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

#include "grob.hh"
#include "lily-guile.hh"
#include "page-layout-problem.hh"

LY_DEFINE (ly_get_spacing_spec, "ly:get-spacing-spec", 2, 0, 0,
           (SCM from_scm, SCM to_scm),
           "Return the spacing spec going between the two given grobs,"
           " @var{from_scm} and @var{to_scm}.")
{
  LY_ASSERT_SMOB (Grob, from_scm, 1);
  LY_ASSERT_SMOB (Grob, to_scm, 2);

  Grob *from = unsmob<Grob> (from_scm);
  Grob *to = unsmob<Grob> (to_scm);

  return Page_layout_problem::get_spacing_spec (from, to, false, 0, 0);
}
