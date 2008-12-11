/*
  pango-select-scheme.cc -- implement Pango descr <-> string bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "config.hh"

#if HAVE_PANGO_FT2
#include "pango-font.hh"

LY_DEFINE (ly_make_pango_description_string, "ly:make-pango-description-string",
	   2, 0, 0, (SCM chain, SCM size),
	   "Make a @code{PangoFontDescription} string for the property"
	   " alist @var{chain} at size @var{size}.")
{
  LY_ASSERT_TYPE (scm_is_number, size, 1);
  PangoFontDescription *pfd = properties_to_pango_description (chain, scm_to_double (size));
  char *str = pango_font_description_to_string (pfd);

  SCM scm_str = scm_from_locale_string (str);
  g_free (str);
  pango_font_description_free (pfd);
  return scm_str;
}

#endif
