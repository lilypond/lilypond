/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2008--2023 Han-Wen Nienhuys <hanwen@lilypond.org>


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
