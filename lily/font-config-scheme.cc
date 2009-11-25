/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "lily-guile.hh"
#include "international.hh"
#include "main.hh"
#include "string-convert.hh"
#include "warn.hh"

#include <fontconfig/fontconfig.h>

string
display_fontset (FcFontSet *fs)
{
  string retval;

  int j;
  for (j = 0; j < fs->nfont; j++)
    {
      FcChar8 *font;
      FcChar8 *str;

      font = FcNameUnparse (fs->fonts[j]);
      if (FcPatternGetString (fs->fonts[j], FC_FILE, 0, &str) == FcResultMatch)
	retval += String_convert::form_string ("FILE %s\n", str);
      if (FcPatternGetString (fs->fonts[j], FC_INDEX, 0, &str) == FcResultMatch)
	retval += String_convert::form_string ("INDEX %s\n", str);
      if (FcPatternGetString (fs->fonts[j], FC_FAMILY, 0, &str) == FcResultMatch)
	retval += String_convert::form_string ("family %s\n ", str);
      if (FcPatternGetString (fs->fonts[j],
			      "designsize", 0, &str) == FcResultMatch)
	retval += String_convert::form_string ("designsize %s\n ", str);

      retval += String_convert::form_string ("%s\n", (const char *)font);
      free (font);
    }

  return retval;
}

string
display_strlist (char const *what, FcStrList *slist)
{
  string retval;
  while (FcChar8 *dir = FcStrListNext (slist))
    {
      retval += String_convert::form_string ("%s: %s\n", what, dir);
    }
  return retval;
}

string
display_config (FcConfig *fcc)
{
  string retval;
  retval += display_strlist ("Config files", FcConfigGetConfigFiles (fcc));
  retval += display_strlist ("Config dir", FcConfigGetConfigDirs (fcc));
  retval += display_strlist ("Font dir", FcConfigGetFontDirs (fcc));
  return retval;
}

string
display_list (FcConfig *fcc)
{
  FcPattern *pat = FcPatternCreate ();

  FcObjectSet *os = 0;
  if (!os)
    os = FcObjectSetBuild (FC_FAMILY, FC_STYLE, (char *)0);

  FcFontSet *fs = FcFontList (fcc, pat, os);
  FcObjectSetDestroy (os);
  if (pat)
    FcPatternDestroy (pat);

  string retval;
  if (fs)
    {
      retval = display_fontset (fs);
      FcFontSetDestroy (fs);
    }
  return retval;
}


LY_DEFINE (ly_font_config_get_font_file, "ly:font-config-get-font-file", 1, 0, 0,
	   (SCM name),
	   "Get the file for font @var{name}.")
{
  LY_ASSERT_TYPE (scm_is_string, name, 1);

  FcPattern *pat = FcPatternCreate ();
  FcValue val;

  val.type = FcTypeString;
  val.u.s = (const FcChar8 *)ly_scm2string (name).c_str (); // FC_SLANT_ITALIC;
  FcPatternAdd (pat, FC_FAMILY, val, FcFalse);

  FcResult result;
  SCM scm_result = SCM_BOOL_F;

  FcConfigSubstitute (NULL, pat, FcMatchFont);
  FcDefaultSubstitute (pat);

  pat = FcFontMatch (NULL, pat, &result);
  FcChar8 *str = 0;
  if (FcPatternGetString (pat, FC_FILE, 0, &str) == FcResultMatch)
    scm_result = scm_from_locale_string ((char const *)str);

  FcPatternDestroy (pat);

  return scm_result;
}
	
LY_DEFINE (ly_font_config_display_fonts, "ly:font-config-display-fonts", 0, 0, 0,
	   (),
	   "Dump a list of all fonts visible to FontConfig.")
{
  string str = display_list (NULL);
  str += display_config (NULL);

  progress_indication (str);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_font_config_add_directory, "ly:font-config-add-directory", 1, 0, 0,
	   (SCM dir),
	   "Add directory @var{dir} to FontConfig.")
{
  LY_ASSERT_TYPE (scm_is_string, dir, 1);

  string d = ly_scm2string (dir);

  if (!FcConfigAppFontAddDir (0, (const FcChar8 *)d.c_str ()))
    error (_f ("failed adding font directory: %s", d.c_str ()));
  else if (be_verbose_global)
    message (_f ("adding font directory: %s", d.c_str ()));

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_font_config_add_font, "ly:font-config-add-font", 1, 0, 0,
	   (SCM font),
	   "Add font @var{font} to FontConfig.")
{
  LY_ASSERT_TYPE (scm_is_string, font, 1);

  string f = ly_scm2string (font);

  if (!FcConfigAppFontAddFile (0, (const FcChar8 *)f.c_str ()))
    error (_f ("failed adding font file: %s", f.c_str ()));
  else if (be_verbose_global)
    message (_f ("adding font file: %s", f.c_str ()));

  return SCM_UNSPECIFIED;
}
