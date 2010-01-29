/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2010 Han-Wen Nienhuys <hanwen@lilypond.org>
  

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

#include "all-font-metrics.hh"
#include "main.hh"

LY_DEFINE (ly_reset_all_fonts, "ly:reset-all-fonts", 0, 0, 0,
	   (),
	   "Forget all about previously loaded fonts.")
{
  delete all_fonts_global;
  all_fonts_global = new All_font_metrics (global_path.to_string ());

  return SCM_UNSPECIFIED;
}


LY_DEFINE (ly_system_font_load, "ly:system-font-load", 1, 0, 0,
	   (SCM name),
	   "Load the OpenType system font @file{@var{name}.otf}."
	   "  Fonts loaded with this command must contain three"
	   " additional SFNT font tables called @code{LILC},"
	   " @code{LILF}, and @code{LILY}, needed for typesetting"
	   " musical elements.  Currently, only the Emmentaler and"
	   " the Aybabtu fonts fulfill these requirements.\n"
	   "\n"
	   "Note that only @code{ly:font-get-glyph} and derived"
	   " code (like @code{\\lookup}) can access glyphs from"
	   " the system fonts; text strings are handled exclusively"
	   " via the Pango interface.")
{
  LY_ASSERT_TYPE (scm_is_string, name, 1);
  
  string name_str = ly_scm2string (name);
  Font_metric *fm = all_fonts_global->find_font (name_str);

  return fm->self_scm ();
}
