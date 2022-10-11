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

#include "freetype.hh"

#include FT_TRUETYPE_TABLES_H

#include "international.hh"
#include "warn.hh"
#include "lily-guile.hh"
#include "open-type-font.hh"

#include <cstdio>
#include <sstream>

using std::string;
using std::vector;

Index_to_charcode_map
make_index_to_charcode_map (FT_Face face)
{
  Index_to_charcode_map m;
  FT_ULong charcode;
  FT_UInt gindex;

  FT_CharMap current_cmap = face->charmap;
  FT_Select_Charmap (face, FT_ENCODING_UNICODE);

  int j = 0;
  for (charcode = FT_Get_First_Char (face, &gindex); gindex != 0;
       charcode = FT_Get_Next_Char (face, charcode, &gindex))
    {
      m[gindex] = charcode;
      j++;
    }
  FT_Set_Charmap (face, current_cmap);

  return m;
}

/*
  Based on ttfps by Juliusz Chroboczek
*/
static void
print_header (std::ostream &stream, FT_Face face)
{
  stream << "%!PS-TrueTypeFont" << std::endl;

  const auto *const pt
    = static_cast<TT_Postscript *> (FT_Get_Sfnt_Table (face, ft_sfnt_post));

  if (pt->maxMemType42)
    stream << "%%VMUsage: 0 0" << std::endl;

  stream << "11 dict begin" << std::endl
         << "/FontName /" << get_postscript_name (face) << " def" << std::endl;

  stream << "/Encoding StandardEncoding def" << std::endl
         << "/PaintType 0 def" << std::endl
         << "/FontMatrix [1 0 0 1 0 0] def" << std::endl;

  const auto *const ht
    = static_cast<TT_Header *> (FT_Get_Sfnt_Table (face, ft_sfnt_head));

  stream << "/FontBBox [" << float (ht->xMin) / float (ht->Units_Per_EM) << " "
         << float (ht->yMin) / float (ht->Units_Per_EM) << " "
         << float (ht->xMax) / float (ht->Units_Per_EM) << " "
         << float (ht->yMax) / float (ht->Units_Per_EM) << "] def" << std::endl;

  stream << "/FontType 42 def" << std::endl
         << "/FontInfo 8 dict dup begin" << std::endl;
  // This explicit cast avoids a long-to-double conversion warning on
  // architectures with longer longs.
  int32_t font_revision = static_cast<int32_t> (ht->Font_Revision);
  stream << "/version (" << (font_revision / 65536.0) << ") def" << std::endl;

  stream << "/isFixedPitch " << (pt->isFixedPitch ? "true" : "false") << " def"
         << std::endl
         << "/UnderlinePosition "
         << float (pt->underlinePosition) / float (ht->Units_Per_EM) << " def"
         << std::endl
         << "/UnderlineThickness "
         << float (pt->underlineThickness) / float (ht->Units_Per_EM) << " def"
         << std::endl
         << "end readonly def" << std::endl;
}

#define CHUNKSIZE 65534

const FT_ULong FT_ENC_TAG (glyf_tag, 'g', 'l', 'y', 'f');
const FT_ULong FT_ENC_TAG (head_tag, 'h', 'e', 'a', 'd');
const FT_ULong FT_ENC_TAG (loca_tag, 'l', 'o', 'c', 'a');

static void
t42_write_table (std::ostream &stream, FT_Face face,
                 unsigned char const *buffer, size_t s, bool is_glyf,
                 FT_ULong head_length, FT_ULong loca_length)
{
  vector<FT_UShort> chunks;
  bool long_offsets = false;
  FT_Error error;

  if (is_glyf)
    {
      // check whether long offsets are used in the `loca` table
      unsigned char *head_buf = new unsigned char[head_length];
      error = FT_Load_Sfnt_Table (face, head_tag, 0, head_buf, NULL);
      if (error)
        programming_error ("FT_Load_Sfnt_Table (): error.");

      /* we access the lower byte of indexToLocFormat */
      long_offsets = head_buf[4 * 4 + 2 * 2 + 2 * 8 + 4 * 2 + 3 * 2 + 1] == 1;

      delete[] head_buf;
    }

  // We split the `glyf` table into chunks aligned on glyph boundaries only
  // if there are short offsets.  The Type 42 specification expects that
  // offsets (and thus chunk lengths) are always even; this cannot be
  // guaranteed for long offsets, as counterexamples like the Korean Baekmuk
  // fonts demonstrate.  Note that this splitting is done only for
  // compatibility with PS interpreters earlier than version 2013, which was
  // released in 1993...
  if (is_glyf && !long_offsets)
    {
      /* compute chunk sizes */
      unsigned char *loca_buf = new unsigned char[loca_length];
      error = FT_Load_Sfnt_Table (face, loca_tag, 0, loca_buf, NULL);
      if (error)
        programming_error ("FT_Load_Sfnt_Table (): error.");

      unsigned char *p = loca_buf;
      unsigned char *endp = loca_buf + loca_length;

      FT_ULong offset = 0, last_offset = 0, last_chunk = 0;
      while (p < endp)
        {
          offset = ((p[0] << 8) | p[1]) << 1;
          p += 2;

          if (offset > last_offset + CHUNKSIZE)
            {
              if (last_chunk != last_offset)
                chunks.push_back (FT_UShort (last_offset - last_chunk));
              /*
                a single glyph with more than 64k data
                is a pathological case but...
               */
              FT_ULong rest = offset - last_offset;
              while (rest > CHUNKSIZE)
                {
                  chunks.push_back (CHUNKSIZE);
                  rest -= CHUNKSIZE;
                }
              chunks.push_back (FT_UShort (rest));
              last_chunk = offset;
            }
          else if (offset > last_chunk + CHUNKSIZE)
            {
              chunks.push_back (FT_UShort (last_offset - last_chunk));
              last_chunk = last_offset;
            }

          last_offset = offset;
        }
      chunks.push_back (FT_UShort (s - last_chunk));

      delete[] loca_buf;
    }
  else if (s > CHUNKSIZE)
    {
      FT_ULong rest = s;
      while (rest > CHUNKSIZE)
        {
          chunks.push_back (CHUNKSIZE);
          rest -= CHUNKSIZE;
        }
      chunks.push_back (FT_UShort (rest));
    }
  else
    chunks.push_back (CHUNKSIZE);

  stream << std::endl << " <";

  int l = 0;
  static char xdigits[] = "0123456789ABCDEF";

  int cur_chunk_idx = 0;
  for (size_t j = 0; j < s; j++)
    {
      if (l >= chunks[cur_chunk_idx])
        {
          stream << std::endl << " 00>" << std::endl << " <";
          l = 0;
          cur_chunk_idx++;
        }

      if (l % 31 == 0)
        stream << std::endl << "  ";

      stream << xdigits[(buffer[j] & 0xF0) >> 4] << xdigits[buffer[j] & 0x0F];

      l++;
    }

  /* pad to four-byte boundary */
  while ((s++) % 4 != 0)
    stream << "00";

  stream << std::endl << "  00" << std::endl << " >";
}

static void
print_body (std::ostream &stream, FT_Face face)
{
  FT_UInt idx = 0;
  FT_ULong head_length = 0, loca_length = 0;
  FT_ULong tag, length;
  vector<FT_ULong> lengths, tags;

  /*
    we must build our own TTF header -- the original font
    might be a TTC where tables are not contiguous, or the font
    contains tables which aren't indexed at all
   */
  while (FT_Sfnt_Table_Info (face, idx, &tag, &length) != FT_Err_Table_Missing)
    {
      lengths.push_back (length);
      tags.push_back (tag);
      if (tag == head_tag)
        head_length = length;
      else if (tag == loca_tag)
        loca_length = length;
      idx++;
    }

  FT_ULong hlength = 12 + 16 * idx;

  unsigned char *hbuf = new unsigned char[hlength];
  unsigned char *p;

  hbuf[0] = 0x00; /* version */
  hbuf[1] = 0x01;
  hbuf[2] = 0x00;
  hbuf[3] = 0x00;
  hbuf[4] = static_cast<unsigned char> ((idx & 0xFF00) >> 8); /* numTables */
  hbuf[5] = idx & 0x00FF;

  FT_UInt searchRange, entrySelector, rangeShift;
  FT_UInt i, j;
  for (i = 1, j = 2; j <= idx; i++, j <<= 1)
    ;
  entrySelector = i - 1;
  searchRange = 0x10 << entrySelector;
  rangeShift = (idx << 4) - searchRange;

  hbuf[6] = static_cast<unsigned char> ((searchRange & 0xFF00) >> 8);
  hbuf[7] = searchRange & 0x00FF;
  hbuf[8] = static_cast<unsigned char> ((entrySelector & 0xFF00) >> 8);
  hbuf[9] = entrySelector & 0x00FF;
  hbuf[10] = static_cast<unsigned char> ((rangeShift & 0xFF00) >> 8);
  hbuf[11] = rangeShift & 0x00FF;

  p = &hbuf[12];

  FT_ULong checksum, font_checksum = 0;

  FT_ULong offset = hlength; /* first table offset */

  for (FT_UInt i = 0; i < idx; i++)
    {
      /* here, the buffer length must be a multiple of 4 */
      FT_ULong len = (lengths[i] + 3) & ~3;
      unsigned char *buf = new unsigned char[len];

      buf[len - 1] = 0x00; /* assure padding with zeros */
      buf[len - 2] = 0x00;
      buf[len - 3] = 0x00;

      FT_Error error = FT_Load_Sfnt_Table (face, tags[i], 0, buf, NULL);
      if (error)
        programming_error ("FT_Load_Sfnt_Table (): error.");

      if (tag == head_tag)
        {
          /*
            first pass of computing the font checksum
            needs checkSumAdjustment = 0
           */
          buf[8] = 0x00;
          buf[9] = 0x00;
          buf[10] = 0x00;
          buf[11] = 0x00;
        }

      checksum = 0;
      unsigned char *endq = buf + len;
      for (unsigned char *q = buf; q < endq; q += 4)
        checksum += (q[0] << 24) | (q[1] << 16) | (q[2] << 8) | q[3];
      font_checksum += checksum;

      delete[] buf;

      *(p++) = static_cast<unsigned char> ((tags[i] & 0xFF000000UL) >> 24);
      *(p++) = static_cast<unsigned char> ((tags[i] & 0x00FF0000UL) >> 16);
      *(p++) = static_cast<unsigned char> ((tags[i] & 0x0000FF00UL) >> 8);
      *(p++) = tags[i] & 0x000000FFUL;

      *(p++) = static_cast<unsigned char> ((checksum & 0xFF000000UL) >> 24);
      *(p++) = static_cast<unsigned char> ((checksum & 0x00FF0000UL) >> 16);
      *(p++) = static_cast<unsigned char> ((checksum & 0x0000FF00UL) >> 8);
      *(p++) = checksum & 0x000000FFUL;

      *(p++) = static_cast<unsigned char> ((offset & 0xFF000000UL) >> 24);
      *(p++) = static_cast<unsigned char> ((offset & 0x00FF0000UL) >> 16);
      *(p++) = static_cast<unsigned char> ((offset & 0x0000FF00UL) >> 8);
      *(p++) = offset & 0x000000FFUL;

      *(p++) = static_cast<unsigned char> ((lengths[i] & 0xFF000000UL) >> 24);
      *(p++) = static_cast<unsigned char> ((lengths[i] & 0x00FF0000UL) >> 16);
      *(p++) = static_cast<unsigned char> ((lengths[i] & 0x0000FF00UL) >> 8);
      *(p++) = lengths[i] & 0x000000FFUL;

      /* offset must be a multiple of 4 */
      offset += (lengths[i] + 3) & ~3;
    }

  /* add checksum of TTF header */
  checksum = 0;
  for (unsigned char *q = hbuf; q < p; q += 4)
    checksum += (q[0] << 24) | (q[1] << 16) | (q[2] << 8) | q[3];
  font_checksum += checksum;
  font_checksum = 0xB1B0AFBAUL - font_checksum;

  /*
    see Adobe technical note 5012.Type42_Spec.pdf for details how
    the /sfnts array must be constructed
   */
  stream << "/sfnts [";
  t42_write_table (stream, face, hbuf, hlength, false, head_length,
                   loca_length);
  delete[] hbuf;

  idx = 0;

  while (FT_Sfnt_Table_Info (face, idx, &tag, &length) != FT_Err_Table_Missing)
    {
      unsigned char *buf = new unsigned char[length];
      FT_Error error = FT_Load_Sfnt_Table (face, tag, 0, buf, NULL);
      if (error)
        programming_error ("FT_Load_Sfnt_Table (): error.");

      if (tag == head_tag)
        {
          /* in the second pass simply store the computed font checksum */
          buf[8]
            = static_cast<unsigned char> ((font_checksum & 0xFF000000UL) >> 24);
          buf[9]
            = static_cast<unsigned char> ((font_checksum & 0x00FF0000UL) >> 16);
          buf[10]
            = static_cast<unsigned char> ((font_checksum & 0x0000FF00UL) >> 8);
          buf[11] = font_checksum & 0x000000FFUL;
        }

      bool is_glyf_table = tag == glyf_tag && length > CHUNKSIZE;
      t42_write_table (stream, face, buf, length, is_glyf_table, head_length,
                       loca_length);

      delete[] buf;
      idx++;
    }
  stream << "\n] def" << std::endl;
}

static void
print_trailer (std::ostream &stream, FT_Face face)
{
  const int GLYPH_NAME_LEN = 256;
  char glyph_name[GLYPH_NAME_LEN];

  const auto *const mp
    = static_cast<TT_MaxProfile *> (FT_Get_Sfnt_Table (face, ft_sfnt_maxp));

  stream << "/CharStrings " << mp->numGlyphs << " dict dup begin" << std::endl;

  Index_to_charcode_map ic_map (make_index_to_charcode_map (face));

  int output_count = 0;
  for (int i = 0; i < mp->numGlyphs; i++)
    {
      glyph_name[0] = 0;
      if (face->face_flags & FT_FACE_FLAG_GLYPH_NAMES)
        {
          FT_Error error
            = FT_Get_Glyph_Name (face, i, glyph_name, GLYPH_NAME_LEN);
          if (error)
            {
              programming_error ("FT_Get_Glyph_Name (): error.");
              glyph_name[0] = 0;
            }
        }

      if (!glyph_name[0] && ic_map.find (i) != ic_map.end ())
        {
          FT_ULong ucode = ic_map[i];
          get_unicode_name (glyph_name, ucode);
        }

      if (i == 0)
        sprintf (glyph_name, ".notdef");
      else if (glyph_name == string (".notdef"))
        glyph_name[0] = '\0';

      if (!glyph_name[0])
        get_glyph_index_name (glyph_name, i);

      if (glyph_name[0])
        {
          stream << "(" << glyph_name << ") cvn " << i << " def ";
          output_count++;
        }
      else
        programming_error (to_string ("no name for glyph %d", i));

      if (!(output_count % 5))
        stream << std::endl;
    }

  stream << "end readonly def" << std::endl;
  stream << "FontName currentdict end definefont pop" << std::endl;
}

static void
create_type42_font (std::ostream &stream, const string &name, int idx)
{
  FT_Face face;

  /* check whether font index is valid */
  if (idx > 0)
    {
      face = open_ft_face (name, -1);
      if (idx >= face->num_faces)
        {
          warning (_f ("font index %d too large for font `%s', using index 0",
                       idx, name.c_str ()));
          idx = 0;
        }
      FT_Done_Face (face);
    }

  face = open_ft_face (name, idx);

  print_header (stream, face);
  print_body (stream, face);
  print_trailer (stream, face);

  FT_Done_Face (face);
}

LY_DEFINE (ly_ttf_ps_name, "ly:ttf-ps-name", 1, 1, 0,
           (SCM ttf_file_name, SCM idx),
           R"(
Extract the PostScript name from a TrueType font.  The optional @var{idx}
argument is useful for TrueType collections (TTC) only; it specifies the font
index within the TTC.  The default value of @var{idx} is@tie{}0.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, ttf_file_name, 1);

  int i = 0;
  if (!SCM_UNBNDP (idx))
    {
      LY_ASSERT_TYPE (scm_is_integer, idx, 2);
      i = from_scm<int> (idx);
      if (i < 0)
        {
          warning (_ ("font index must be non-negative, using index 0"));
          i = 0;
        }
    }

  string file_name = ly_scm2string (ttf_file_name);
  debug_output ("\n[" + file_name, false);

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
  SCM ps_name = scm_from_locale_string (get_postscript_name (face).c_str ());
  FT_Done_Face (face);

  debug_output ("]", false);

  return ps_name;
}

LY_DEFINE (ly_ttf_2_pfa, "ly:ttf->pfa", 1, 1, 0, (SCM ttf_file_name, SCM idx),
           R"(
Convert the contents of a TrueType font file to PostScript Type@tie{}42 font,
returning it as a string.  The optional @var{idx} argument is useful for
TrueType collections (TTC) only; it specifies the font index within the TTC.
The default value of @var{idx} is@tie{}0.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, ttf_file_name, 1);

  int i = 0;
  if (!SCM_UNBNDP (idx))
    {
      LY_ASSERT_TYPE (scm_is_integer, idx, 2);
      i = from_scm<int> (idx);
      if (i < 0)
        {
          warning (_ ("font index must be non-negative, using index 0"));
          i = 0;
        }
    }

  string file_name = ly_scm2string (ttf_file_name);
  debug_output ("[" + file_name); // Debug message should start on a new line

  std::ostringstream stream;
  create_type42_font (stream, file_name, i);

  std::string font = stream.str ();
  SCM asscm = scm_from_latin1_stringn (font.c_str (), font.length ());

  debug_output ("]", false);

  return asscm;
}
