/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "line-interface.hh"

#include "stencil.hh"
#include "grob.hh"

LY_DEFINE (ly_line_interface__line, "ly:line-interface::line", 5, 0, 0,
           (SCM grob, SCM startx, SCM starty, SCM endx, SCM endy),
           R"(
Make a line using layout information from grob @var{grob}.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (scm_is_number, startx, 2);
  LY_ASSERT_TYPE (scm_is_number, starty, 3);
  LY_ASSERT_TYPE (scm_is_number, endx, 4);
  LY_ASSERT_TYPE (scm_is_number, endy, 5);

  Offset from = Offset (from_scm<double> (startx), from_scm<double> (starty));
  Offset to = Offset (from_scm<double> (endx), from_scm<double> (endy));

  Stencil stil = Line_interface::line (me, from, to);

  return stil.smobbed_copy ();
}
