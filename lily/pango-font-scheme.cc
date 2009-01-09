/*
  pango-font-scheme.cc -- implement Pango_font

  source file of the GNU LilyPond music typesetter

  (c) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#define PANGO_ENABLE_BACKEND // ugh, why necessary?

#include <pango/pangoft2.h>

#include "main.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "pango-font.hh"
#include "warn.hh"

#if HAVE_PANGO_FT2
#include "stencil.hh"

LY_DEFINE (ly_pango_font_p, "ly:pango-font?",
	   1, 0, 0,
	   (SCM f),
	   "Is @var{f} a pango font?")
{
  return scm_from_bool (dynamic_cast<Pango_font *> (unsmob_metrics (f)));
}

LY_DEFINE (ly_pango_font_physical_fonts, "ly:pango-font-physical-fonts",
	   1, 0, 0,
	   (SCM f),
	   "Return alist of @code{(ps-name file-name font-index)} lists"
	   " for Pango font@tie{}@var{f}.")
{
  Pango_font *pf = dynamic_cast<Pango_font *> (unsmob_metrics (f));

  SCM alist = SCM_EOL;
  if (pf)
    alist = ly_hash2alist (pf->physical_font_tab ());

  return alist;
}
#endif
