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

#include "paper-score.hh"

LY_DEFINE (ly_paper_score_paper_systems, "ly:paper-score-paper-systems", 1, 0,
           0, (SCM paper_score),
           R"(
Return vector of @code{paper_system} objects from @var{paper-score}.
           )")
{
  auto *const pscore = LY_ASSERT_SMOB (Paper_score, paper_score, 1);

  return pscore->get_paper_systems ();
}
