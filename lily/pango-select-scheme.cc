/*
  pango-select-scheme.cc -- implement Pango descr <-> string bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "config.hh"

#if HAVE_PANGO_FT2
#include "pango-font.hh"

LY_DEFINE (ly_make_pango_description_string, "ly:make-pango-description-string",
	   2, 0, 0, (SCM chain, SCM size),
	   "Make a PangoFontDescription string for the property alist @var{chain} at size @var{size}.")
{
  SCM_ASSERT_TYPE (scm_is_number (size), size, SCM_ARG1, __FUNCTION__, "number");
  PangoFontDescription *pfd = properties_to_pango_description (chain, scm_to_double (size));
  char *str = pango_font_description_to_string (pfd);

  SCM scm_str = scm_makfrom0str (str);
  g_free (str);
  pango_font_description_free (pfd);
  return scm_str;
}

#endif
