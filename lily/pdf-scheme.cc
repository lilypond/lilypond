/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2015 Reinhold Kainhofer <reinhold@kainhofer.com>

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

#include <glib.h>
using namespace std;

#include "international.hh"
#include "warn.hh"
#include "lily-guile.hh"

LY_DEFINE (ly_encode_string_for_pdf, "ly:encode-string-for-pdf",
           1, 0, 0, (SCM str),
           "Encode the given string to either Latin1 (which is a subset of"
           " the PDFDocEncoding) or if that's not possible to full UTF-16BE"
           " with Byte-Order-Mark (BOM).")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  char *p = ly_scm2str0 (str);
  char *g = NULL;
  char const *charset = "UTF-8"; // Input is ALWAYS UTF-8!
  gsize bytes_written = 0;

#if 0

  /* First, try to convert to ISO-8859-1 (no encodings required). This will
   * fail, if the string contains accented characters, so we do not check
   * for errors. */
  g = g_convert (p, -1, "ISO-8859-1", charset, 0, &bytes_written, 0);

#else

  /* In contrast to the above comment, we do _not_ try full ISO-8859-1
   * since a number of Ghostscript versions fail to properly convert
   * this into PDF.  UTF-16BE, in contrast, works better with recent
   * versions of Ghostscript.
   */

  g = g_convert (p, -1, "ASCII", charset, 0, &bytes_written, 0);

#endif

  /* If that fails, we have to resolve to full UTF-16BE */
  if (!g)
    {
      GError *e = NULL;
      char *g_without_BOM = g_convert (p, -1, "UTF-16BE", charset, 0, &bytes_written, &e);
      if (e != NULL)
        {
          warning (_f ("Conversion of string `%s' to UTF-16be failed: %s", p, e->message));
          g_error_free (e);
        }
      /* UTF-16BE allows/recommends a byte-order-mark (BOM) of two bytes
       * \xFE\xFF at the begin of the string. The pdfmark specification
       * requires it and depends on it to distinguish PdfDocEncoding from
       * UTF-16BE. As g_convert does not automatically prepend this BOM
       * for UTF-16BE (only for UTF-16, which uses lower endian by default,
       * though), we have to prepend it manually. */
      if (g_without_BOM) // conversion to UTF-16be might have failed (shouldn't!)
        {
          g = (char *)malloc ( sizeof (char) * (bytes_written + 3));
          char const *BOM = "\xFE\xFF";
          strcpy (g, BOM);
          memcpy (&g[2], g_without_BOM, bytes_written + 1); // Copy string + \0
          g_free (g_without_BOM);
          bytes_written += 2;
        }
    }
  free (p);

  /* Convert back to SCM object and return it */
  if (g)
    {
      /*
       * Return the raw byte representation of the UTF-16BE encoded string,
       * in a locale independent way.
       */
      SCM string = scm_from_latin1_stringn (g, bytes_written);
      free(g);
      return string;
    }
  else
    return str;
}
