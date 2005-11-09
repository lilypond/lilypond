/*
  font-config-scheme.cc -- implement FontConfig bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "lily-guile.hh"

#include <fontconfig/fontconfig.h>

void
display_fontset (FcFontSet *fs)
{
  int j;
  for (j = 0; j < fs->nfont; j++)
    {
      FcChar8 *font;
      FcChar8 *str;

      font = FcNameUnparse (fs->fonts[j]);
      if (FcPatternGetString (fs->fonts[j], FC_FILE, 0, &str) == FcResultMatch)
	printf ("FILE %s\n", str);
      if (FcPatternGetString (fs->fonts[j], FC_FAMILY, 0, &str) == FcResultMatch)
	printf ("family %s\n ", str);
      if (FcPatternGetString (fs->fonts[j],
			      "designsize", 0, &str) == FcResultMatch)
	printf ("designsize %s\n ", str);
      
      printf ("%s\n", font);
      free (font);
    }
}


void
display_list (FcConfig *fcc)
{
  FcPattern*pat = FcPatternCreate ();

  FcObjectSet *os = 0;
  if (!os)
    os = FcObjectSetBuild (FC_FAMILY, FC_STYLE, (char *) 0);

  FcFontSet   * fs = FcFontList (fcc, pat, os);
  FcObjectSetDestroy (os);
  if (pat)
    FcPatternDestroy (pat);

  if (fs)
    {
      display_fontset (fs);
      FcFontSetDestroy (fs);
    }
}


LY_DEFINE (ly_font_config_display_fonts, "ly:font-config-display-fonts", 0, 0, 0,
	   (),
	   "Dump a list of all fonts visible to FontConfig.")

{
  display_list (NULL);
  
  return SCM_UNSPECIFIED;
}
