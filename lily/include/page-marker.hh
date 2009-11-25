/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2009 Nicolas Sceaux <nicolas.sceaux@free.fr>

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

#ifndef PAGE_MARKER_HH
#define PAGE_MARKER_HH

#include "smobs.hh"

class Page_marker
{
  DECLARE_SMOBS (Page_marker);

  SCM symbol_; /* either 'page-turn-permission or 'page-break-permission */
  SCM permission_;  /* 'force, 'allow, or '() */
  SCM label_; /* bookmarking label (a symbol) */

public:
  Page_marker ();
  
  void set_permission (SCM symbol, SCM permission);
  void set_label (SCM label);

  SCM permission_symbol ();
  SCM permission_value ();
  SCM label ();
};

DECLARE_UNSMOB (Page_marker, page_marker)

#endif /* PAGE_MARKER_HH */
