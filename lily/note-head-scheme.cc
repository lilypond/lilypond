/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@lilypond.org>


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

#include "note-head.hh"
#include "font-metric.hh"

LY_DEFINE (ly_note_head__stem_attachment, "ly:note-head::stem-attachment", 2, 1,
           0, (SCM font_metric, SCM glyph_name, SCM direction),
           R"(
Get attachment in @var{font-metric} for attaching a stem to notehead
@var{glyph-name} in the direction @var{direction} (default @code{UP}).
           )")
{
  auto *const fm = LY_ASSERT_SMOB (Font_metric, font_metric, 1);
  LY_ASSERT_TYPE (scm_is_string, glyph_name, 2);

  if (SCM_UNBNDP (direction))
    direction = to_scm (UP);
  else
    LY_ASSERT_TYPE (is_scm<Direction>, direction, 3);

  return to_scm (Note_head::get_stem_attachment (
    fm, ly_scm2string (glyph_name), from_scm<Direction> (direction)));
}
