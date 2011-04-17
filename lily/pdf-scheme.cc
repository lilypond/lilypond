/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011 Reinhold Kainhofer <reinhold@kainhofer.com>

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

#include "lily-guile.hh"


LY_DEFINE (ly_encode_string_for_pdf, "ly:encode-string-for-pdf",
	   1, 0, 0, (SCM str),
	   "Check whether the string needs to be encoded for PDF output (Latin1,"
	   " PDFDocEncoding or in the most general case UTF-16BE).")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  char *p = ly_scm2str0 (str);
  char *g = NULL;
  const char *charset;
  gsize bytes_written = 0;
  g_get_charset (&charset); /* The current locale */

  /* First, try to convert to ISO-8859-1 (no encodings required) */
  g = g_convert (p, -1, "ISO-8859-1", charset, 0, &bytes_written, 0);
  /* If that fails, we have to resolve to full UTF-16BE */
  if (!g) {
    char *g_without_BOM = g_convert (p, -1,  "UTF-16BE", charset, 0, &bytes_written, 0);
    /* prepend the BOM manually, g_convert doesn't do it! */
    g = new char[bytes_written+3];
    g[0] = (char)254;
    g[1] = (char)255;
    memcpy (&g[2], g_without_BOM, bytes_written+1); // Copy string + \0
    free (g_without_BOM);
    bytes_written += 2;
  }
  free (p);

  /* Convert back to SCM object and return it */
  if (g) {
    return scm_from_locale_stringn (g, bytes_written);
  } else {
    return str;
  }

}
