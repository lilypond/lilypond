/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cstdio>

using std::string;

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

LY_DEFINE (ly_has_glyph_names_p, "ly:has-glyph-names?",
           1, 1, 0, (SCM font_file_name, SCM idx),
           "Does the font for @var{font_file_name} have glyph names?"
           "  The optional @var{idx} argument is useful for"
           " TrueType Collections (TTC) and"
           " OpenType/CFF collections (OTC) only;"
           " it specifies the font index within the TTC/OTC."
           "  The default value of @var{idx} is@tie{}0.")
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
  bool has_glyph_names = FT_HAS_GLYPH_NAMES (face);
  FT_Done_Face (face);

  return has_glyph_names ? SCM_BOOL_T : SCM_BOOL_F;
}

LY_DEFINE (ly_get_cff_offset, "ly:get-cff-offset",
           1, 1, 0, (SCM font_file_name, SCM idx),
           "Get the offset of 'CFF' table for @var{font_file_name},"
           " returning it as an integer.  The optional"
           " @var{idx} argument is useful for"
           " OpenType/CFF collections (OTC) only;"
           " it specifies the font index within the OTC."
           "  The default value of @var{idx} is@tie{}0.")
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

  FILE *fp = fopen (file_name.c_str (), "rb");
  if (!fp)
    {
      warning (_f ("cannot open font filename `%s'", file_name.c_str ()));
      return SCM_BOOL_F;
    }

  char buff[4];

  // Read `sfnt version` (for OTF) or `TTCTag` (for OTC)
  if (fread (buff, 4, 1, fp) != 1)
    {
      fclose (fp);
      warning (_f ("cannot read %s of `%s'", "header", file_name.c_str ()));
      return SCM_BOOL_F;
    }

  if (buff[0] == 't' && buff[1] == 't' && buff[2] == 'c' && buff[3] == 'f')
    {
      // For OTC
      // Find subfont

      // Skip `Version`
      fseek (fp, 4, SEEK_CUR);

      // Read `numFonts`
      if (fread (buff, 4, 1, fp) != 1)
        {
          fclose (fp);
          warning (_f ("cannot read %s of `%s'",
                       "numFonts", file_name.c_str ()));
          return SCM_BOOL_F;
        }
      int numfonts
        = static_cast<unsigned char>(buff[0]) << 24
          | static_cast<unsigned char>(buff[1]) << 16
          | static_cast<unsigned char>(buff[2]) << 8
          | static_cast<unsigned char>(buff[3]);

      if ( i > numfonts)
        {
          warning (_f ("font index %d too large for font `%s', using index 0",
                       i, file_name.c_str ()));
          i = 0;
        }

      // Read `OffsetTable[i]`
      if (i)
        fseek (fp, i * 4, SEEK_CUR);
      if (fread (buff, 4, 1, fp) != 1)
        {
          fclose (fp);
          warning (_f ("cannot read %s of `%s'",
                       "OffsetTable", file_name.c_str ()));
          return SCM_BOOL_F;
        }
      unsigned int offset
        = static_cast<unsigned char>(buff[0]) << 24
          | static_cast<unsigned char>(buff[1]) << 16
          | static_cast<unsigned char>(buff[2]) << 8
          | static_cast<unsigned char>(buff[3]);

      // Seek to subfont and skip `sfnt version`
      fseek (fp, offset + 4, SEEK_SET);
    }

  // For OTF or subfont of OTC

  // Read `numTables`
  if (fread (buff, 2, 1, fp) != 1)
    {
      fclose (fp);
      warning (_f ("cannot read %s of `%s'",
                   "numTables", file_name.c_str ()));
      return SCM_BOOL_F;
    }
  int numtables
    = static_cast<unsigned char>(buff[0]) << 8
      | static_cast<unsigned char>(buff[1]);

  // Skip `searchRange`, `entrySelector` and `rangeShift`
  fseek (fp, 6, SEEK_CUR);

  // Read Table Records
  for (int t = 0; t < numtables; t++)
    {
      // Read `tag`
      if (fread (buff, 4, 1, fp) != 1)
        {
          fclose (fp);
          warning (_f ("cannot read %s of `%s'",
                       "tag", file_name.c_str ()));
          return SCM_BOOL_F;
        }

      if (buff[0] == 'C' && buff[1] == 'F' && buff[2] == 'F' && buff[3] == ' ')
        {
          // CFF table is found.

          // Skip `checkSum`
          fseek (fp, 4, SEEK_CUR);

          // Read `offset`
          if (fread (buff, 4, 1, fp) != 1)
            {
              fclose (fp);
              warning (_f ("cannot read %s of `%s'",
                           "CFF offset", file_name.c_str ()));
              return SCM_BOOL_F;
            }
          unsigned int offset
            = static_cast<unsigned char>(buff[0]) << 24
              | static_cast<unsigned char>(buff[1]) << 16
              | static_cast<unsigned char>(buff[2]) << 8
              | static_cast<unsigned char>(buff[3]);

          // Done
          fclose (fp);
          return scm_from_unsigned_integer (offset);
        }

      // For non-CFF table

      // Skip `checkSum`, `offset` and `length`
      fseek (fp, 12, SEEK_CUR);
    }

  fclose (fp);
  warning (_f ("font `%s' index %d does not have `CFF' table",
               file_name.c_str (), i));
  return SCM_BOOL_F;
}
