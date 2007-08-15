/*
  font-config-scheme.cc -- implement FontConfig bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "lily-guile.hh"
#include "std-string.hh"

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
      
      printf ("%s\n", (const char*) font);
      free (font);
    }
}

void
display_strlist (char const*what, FcStrList *slist)
{
  while (FcChar8 *dir = FcStrListNext (slist))
    {
      printf("%s: %s\n", what, dir);
    }
}

void
display_config (FcConfig *fcc)
{
  display_strlist ("Config files", FcConfigGetConfigFiles(fcc));
  display_strlist ("Config dir", FcConfigGetConfigDirs(fcc));
  display_strlist ("Font dir", FcConfigGetFontDirs(fcc));
}

void
display_list (FcConfig *fcc)
{
  FcPattern*pat = FcPatternCreate ();

  FcObjectSet *os = 0;
  if (!os)
    os = FcObjectSetBuild (FC_FAMILY, FC_STYLE, (char *) 0);

  FcFontSet *fs = FcFontList (fcc, pat, os);
  FcObjectSetDestroy (os);
  if (pat)
    FcPatternDestroy (pat);

  if (fs)
    {
      display_fontset (fs);
      FcFontSetDestroy (fs);
    }
}


LY_DEFINE (ly_font_config_get_font_file, "ly:font-config-get-font-file", 1, 0, 0,
	   (SCM name),
	   "Get the file for font @var{name}")
{
  SCM_ASSERT_TYPE (scm_is_string (name), name,
		   SCM_ARG1, __FUNCTION__, "string");

  
  FcPattern*pat = FcPatternCreate ();
  FcValue val;
  
  val.type = FcTypeString;
  val.u.s = (const FcChar8*)ly_scm2string (name).c_str (); // FC_SLANT_ITALIC;
  FcPatternAdd(pat, FC_FAMILY, val, FcFalse);

  FcResult result;
  SCM scm_result = SCM_BOOL_F;

  FcConfigSubstitute (NULL, pat, FcMatchFont);
  FcDefaultSubstitute (pat);
  
  pat = FcFontMatch(NULL, pat, &result);
  FcChar8 *str = 0;
  if (FcPatternGetString (pat, FC_FILE, 0, &str) == FcResultMatch)
    scm_result = scm_makfrom0str ((char const*) str);

  FcPatternDestroy (pat);

  return scm_result;
}
	   
LY_DEFINE (ly_font_config_display_fonts, "ly:font-config-display-fonts", 0, 0, 0,
	   (),
	   "Dump a list of all fonts visible to FontConfig.")
{
  display_list (NULL);
  display_config (NULL);
  
  return SCM_UNSPECIFIED;
}


