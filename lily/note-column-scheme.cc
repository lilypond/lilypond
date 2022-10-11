/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2022 Nicolas Sceaux <nicolas.sceaux@free.fr>

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

#include "note-column.hh"
#include "lily-guile.hh"
#include "grob.hh"

LY_DEFINE (ly_note_column_accidentals, "ly:note-column-accidentals", 1, 0, 0,
           (SCM note_column),
           R"(
Return the @code{AccidentalPlacement} grob from @var{note-column} if any, or
@code{SCM_EOL} otherwise.
           )")
{
  auto *const grob = LY_ASSERT_SMOB (Grob, note_column, 1);
  Grob *acc = Note_column::accidentals (grob);
  if (acc)
    return acc->self_scm ();
  return SCM_EOL;
}

LY_DEFINE (ly_note_column_dot_column, "ly:note-column-dot-column", 1, 0, 0,
           (SCM note_column),
           R"(
Return the @code{DotColumn} grob from @var{note-column} if any, or
@code{SCM_EOL} otherwise.
           )")
{
  auto *const grob = LY_ASSERT_SMOB (Grob, note_column, 1);
  Grob *dot_column = Note_column::dot_column (grob);
  if (dot_column)
    return dot_column->self_scm ();
  return SCM_EOL;
}
