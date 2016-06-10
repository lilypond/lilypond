/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "international.hh"
#include "modified-font-metric.hh"
#include "open-type-font.hh"
#include "freetype.hh"

#ifdef FT_FONT_FORMATS_H
/* FreeType 2.6+ */
#include FT_FONT_FORMATS_H
#else
/* FreeType 2.5.5 and earlier */
#include FT_XFREE86_H
#define FT_Get_Font_Format FT_Get_X11_Font_Format
#endif

LY_DEFINE (ly_font_sub_fonts, "ly:font-sub-fonts", 1, 0, 0,
           (SCM font),
           "Given the font metric @var{font} of an OpenType font, return the"
           " names of the subfonts within @var{font}.")
{
  LY_ASSERT_SMOB (Font_metric, font, 1);

  Font_metric *fm = unsmob<Font_metric> (font);
  return fm->sub_fonts ();
}

LY_DEFINE (ly_otf_font_glyph_info, "ly:otf-font-glyph-info", 2, 0, 0,
           (SCM font, SCM glyph),
           "Given the font metric @var{font} of an OpenType font, return the"
           " information about named glyph @var{glyph} (a string).")
{
  Modified_font_metric *fm
    = unsmob<Modified_font_metric> (font);
  Open_type_font *otf = fm
                        ? dynamic_cast<Open_type_font *> (fm->original_font ())
                        : unsmob<Open_type_font> (font);

  SCM_ASSERT_TYPE (otf, font, SCM_ARG1, __FUNCTION__, "OpenType font");
  LY_ASSERT_TYPE (scm_is_string, glyph, 2);

  SCM sym = scm_string_to_symbol (glyph);
  return scm_hashq_ref (otf->get_char_table (), sym, SCM_EOL);
}

LY_DEFINE (ly_otf_font_table_data, "ly:otf-font-table-data", 2, 0, 0,
           (SCM font, SCM tag),
           "Extract a table @var{tag} from @var{font}.  Return empty string"
           " for non-existent @var{tag}.")
{
  Modified_font_metric *fm
    = unsmob<Modified_font_metric> (font);
  Open_type_font *otf = fm
                        ? dynamic_cast<Open_type_font *> (fm->original_font ())
                        : unsmob<Open_type_font> (font);

  SCM_ASSERT_TYPE (otf, font, SCM_ARG1, __FUNCTION__, "OpenType font");
  LY_ASSERT_TYPE (scm_is_string, tag, 2);

  char ctag [5] = "    ";

  string tag_string = ly_scm2string (tag);
  strncpy (ctag, tag_string.c_str (), tag_string.length ());

  string tab = otf->get_otf_table (string (ctag));

  return scm_from_latin1_stringn ((char const *) tab.data (), tab.length ());
}

LY_DEFINE (ly_otf_font_p, "ly:otf-font?", 1, 0, 0,
           (SCM font),
           "Is @var{font} an OpenType font?")
{
  Modified_font_metric *fm
    = unsmob<Modified_font_metric> (font);
  Open_type_font *otf = fm
                        ? dynamic_cast<Open_type_font *> (fm->original_font ())
                        : unsmob<Open_type_font> (font);

  return scm_from_bool (otf);
}

LY_DEFINE (ly_otf_glyph_count, "ly:otf-glyph-count", 1, 0, 0,
           (SCM font),
           "Return the number of glyphs in @var{font}.")
{
  Modified_font_metric *fm
    = unsmob<Modified_font_metric> (font);
  Open_type_font *otf = fm
                        ? dynamic_cast<Open_type_font *> (fm->original_font ())
                        : unsmob<Open_type_font> (font);

  SCM_ASSERT_TYPE (otf, font, SCM_ARG1, __FUNCTION__, "OpenType font");

  return scm_from_int ((int) otf->count ());
}

LY_DEFINE (ly_otf_glyph_list, "ly:otf-glyph-list", 1, 0, 0,
           (SCM font),
           "Return a list of glyph names for @var{font}.")
{
  Modified_font_metric *fm
    = unsmob<Modified_font_metric> (font);
  Open_type_font *otf = fm
                        ? dynamic_cast<Open_type_font *> (fm->original_font ())
                        : unsmob<Open_type_font> (font);

  SCM_ASSERT_TYPE (otf, font, SCM_ARG1, __FUNCTION__, "OpenType font");

  return otf->glyph_list ();
}

LY_DEFINE (ly_get_font_format, "ly:get-font-format",
           1, 1, 0, (SCM font_file_name, SCM idx),
           "Get the font format for @var{font_file_name},"
           " returning it as a symbol.  The optional"
           " @var{idx} argument is useful for TrueType Collections (TTC) and"
           " OpenType/CFF collections (OTC) only;"
           " it specifies the font index within the TTC/OTC."
           " The default value of @var{idx} is@tie{}0.")
{
  LY_ASSERT_TYPE (scm_is_string, font_file_name, 1);

  int i = 0;
  if (!SCM_UNBNDP (idx))
    {
      LY_ASSERT_TYPE (scm_is_integer, idx, 2);
      i = scm_to_int (idx);
      if (i < 0)
        {
          warning (_ ("font index must be non-negative, using index 0"));
          i = 0;
        }
    }

  string file_name = ly_scm2string (font_file_name);

  FT_Face face;
  /* check whether font index is valid */
  if (i > 0)
    {
      face = open_ft_face (file_name, -1);
      if (i >= face->num_faces)
        {
          warning (_f ("font index %d too large for font `%s', using index 0",
                       i, file_name.c_str ()));
          i = 0;
        }
      FT_Done_Face (face);
    }

  face = open_ft_face (file_name, i);
  SCM asscm = scm_from_ascii_symbol (FT_Get_Font_Format (face));
  FT_Done_Face (face);

  return asscm;
}
