/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2010 Han-Wen Nienhuys <hanwen@lilypond.org>


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


LY_DEFINE (ly_note_head__stem_attachment, "ly:note-head::stem-attachment",
	  2, 0, 0, (SCM font_metric, SCM glyph_name),
	  "Get attachment in @var{font-metric} for attaching a stem to"
	  " notehead @var{glyph-name}.")
{
  LY_ASSERT_SMOB (Font_metric, font_metric, 1);
  Font_metric *fm = unsmob_metrics (font_metric);
  LY_ASSERT_TYPE (scm_is_string, glyph_name, 2);
  
  return ly_offset2scm (Note_head::get_stem_attachment (fm, ly_scm2string (glyph_name)));
}

