/*
  page-marker-scheme.cc -- implement Page_marker bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2007--2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
*/

#include "page-marker.hh"

LY_DEFINE (ly_make_page_permission_marker, "ly:make-page-permission-marker",
	   2, 0, 0,
	   (SCM symbol, SCM permission),
	   "Return page marker with page breaking and turning permissions.")
{
  LY_ASSERT_TYPE (ly_is_symbol, symbol, 1);
  Page_marker *page_marker = new Page_marker ();
  page_marker->set_permission (symbol, permission);
  return page_marker->unprotect ();
}

LY_DEFINE (ly_make_page_label_marker, "ly:make-page-label-marker",
	   1, 0, 0,
	   (SCM label),
	   "Return page marker with label.")
{
  LY_ASSERT_TYPE (ly_is_symbol, label, 1);
  Page_marker *page_marker = new Page_marker ();
  page_marker->set_label (label);
  return page_marker->unprotect ();
}
