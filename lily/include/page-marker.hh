/*
  page-marker.hh -- declare Page_marker

  source file of the GNU LilyPond music typesetter

  (c) 2007 Nicolas Sceaux <nicolas.sceaux@free.fr>
*/

#ifndef PAGE_MARKER_HH
#define PAGE_MARKER_HH

#include "smobs.hh"

class Page_marker
{
  DECLARE_SMOBS (Page_marker);

  SCM symbol_; /* either 'page-turn-permission or 'page-break-permission */
  SCM permission_;  /* 'force, 'allow, or '() */

public:
  Page_marker (SCM symbol, SCM permission);
  
  SCM permission_symbol ();
  SCM permission_value ();
};

DECLARE_UNSMOB (Page_marker, page_marker)

#endif /* PAGE_MARKER_HH */
