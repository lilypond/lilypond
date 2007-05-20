/*
  page-marker-scheme.cc -- implement Page_marker bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2007 Nicolas Sceaux <nicolas.sceaux@free.fr>
*/

#include "page-marker.hh"

LY_DEFINE (ly_make_page_marker, "ly:make-page-marker",
	   2, 0, 0,
	   (SCM symbol, SCM permission),
	   "Return page marker with page breaking and turning permissions.")
{
  LY_ASSERT_TYPE (ly_is_symbol, symbol, 1);
  Page_marker *page_marker = new Page_marker (symbol, permission);
  return page_marker->unprotect ();
}
