/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "font-metric.hh"

#include "modified-font-metric.hh"
#include "stencil.hh"
#include "warn.hh"

LY_DEFINE (ly_font_get_glyph, "ly:font-get-glyph", 2, 0, 0,
           (SCM font, SCM name),
           "Return a stencil from @var{font} for the glyph named @var{name}."
           "  If the glyph is not available, return an empty stencil.\n"
           "\n"
           "Note that this command can only be used to access glyphs from"
           " fonts loaded with @code{ly:system-font-load}; currently, this"
           " means either the Emmentaler or Emmentaler-Brace "
           " fonts, corresponding"
           " to the font encodings @code{fetaMusic} and @code{fetaBraces},"
           " respectively.")
{
  Font_metric *fm = unsmob<Font_metric> (font);
  LY_ASSERT_SMOB (Font_metric, font, 1);
  LY_ASSERT_TYPE (scm_is_string, name, 2);

  Stencil m = fm->find_by_name (ly_scm2string (name));

  /* TODO: make optional argument for default if not found.  */
  return m.smobbed_copy ();
}

LY_DEFINE (
    ly_font_glyph_name_to_index, "ly:font-glyph-name-to-index", 2, 0, 0,
    (SCM font, SCM name),
    "Return the index for @var{name} in @var{font}.\n"
    "\n"
    "Note that this command can only be used to access glyphs from"
    " fonts loaded with @code{ly:system-font-load}; currently, this"
    " means either the Emmentaler or Emmentaler-Brace fonts, corresponding"
    " to the font encodings @code{fetaMusic} and @code{fetaBraces},"
    " respectively.")
{
  Font_metric *fm = unsmob<Font_metric> (font);
  LY_ASSERT_SMOB (Font_metric, font, 1);
  LY_ASSERT_TYPE (scm_is_string, name, 2);

  size_t glyph_index = fm->name_to_index (ly_scm2string (name));
  if (glyph_index != GLYPH_INDEX_INVALID)
    return scm_from_size_t (glyph_index);
  else
    return scm_from_int (-1);
}

LY_DEFINE (
    ly_font_index_to_charcode, "ly:font-index-to-charcode", 2, 0, 0,
    (SCM font, SCM index),
    "Return the character code for @var{index} in @var{font}.\n"
    "\n"
    "Note that this command can only be used to access glyphs from"
    " fonts loaded with @code{ly:system-font-load}; currently, this"
    " means either the Emmentaler or Emmentaler-Brace fonts, corresponding"
    " to the font encodings @code{fetaMusic} and @code{fetaBraces},"
    " respectively.")
{
  Font_metric *fm = unsmob<Font_metric> (font);
  LY_ASSERT_SMOB (Font_metric, font, 1);
  LY_ASSERT_TYPE (scm_is_integer, index, 2);

  int i = scm_to_int (index);
  size_t glyph_index ((i >= 0) ? i : GLYPH_INDEX_INVALID);
  size_t charcode = fm->index_to_charcode (glyph_index);
  return scm_from_size_t (charcode);
}

LY_DEFINE (
    ly_font_glyph_name_to_charcode, "ly:font-glyph-name-to-charcode", 2, 0, 0,
    (SCM font, SCM name),
    "Return the character code for glyph @var{name} in @var{font}.\n"
    "\n"
    "Note that this command can only be used to access glyphs from"
    " fonts loaded with @code{ly:system-font-load}; currently, this"
    " means either the Emmentaler or Emmentaler-Brace fonts, corresponding"
    " to the font encodings @code{fetaMusic} and @code{fetaBraces},"
    " respectively.")
{
  Font_metric *fm = unsmob<Font_metric> (font);
  LY_ASSERT_SMOB (Font_metric, font, 1);
  LY_ASSERT_TYPE (scm_is_string, name, 2);

  return scm_from_unsigned_integer (
      fm->index_to_charcode (fm->name_to_index (ly_scm2string (name))));
}

/*
  TODO: when are non string retvals allowed?
 */
LY_DEFINE (ly_font_file_name, "ly:font-file-name", 1, 0, 0, (SCM font),
           "Given the font metric @var{font},"
           " return the corresponding file name.")
{
  LY_ASSERT_SMOB (Font_metric, font, 1);

  Font_metric *fm = unsmob<Font_metric> (font);
  SCM name = fm->font_file_name ();

  return name;
}

LY_DEFINE (ly_font_name, "ly:font-name", 1, 0, 0, (SCM font),
           "Given the font metric @var{font},"
           " return the corresponding name.")
{
  LY_ASSERT_SMOB (Font_metric, font, 1);
  Font_metric *fm = unsmob<Font_metric> (font);

  return ly_string2scm (fm->font_name ());
}

LY_DEFINE (ly_font_magnification, "ly:font-magnification", 1, 0, 0, (SCM font),
           "Given the font metric @var{font}, return the"
           " magnification, relative to the current output-scale.")
{
  LY_ASSERT_SMOB (Font_metric, font, 1);

  Font_metric *fm = unsmob<Font_metric> (font);
  return scm_cdr (fm->description_);
}

LY_DEFINE (ly_font_design_size, "ly:font-design-size", 1, 0, 0, (SCM font),
           "Given the font metric @var{font}, return the"
           " design size, relative to the current output-scale.")
{
  LY_ASSERT_SMOB (Font_metric, font, 1);

  Font_metric *fm = unsmob<Font_metric> (font);
  return scm_from_double (fm->design_size ());
}
