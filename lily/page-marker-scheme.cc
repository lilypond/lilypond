/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Nicolas Sceaux <nicolas.sceaux@free.fr>

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

#include "page-marker.hh"

LY_DEFINE (ly_make_page_permission_marker, "ly:make-page-permission-marker", 2,
           0, 0, (SCM symbol, SCM permission),
           R"(
Return page marker with page breaking and turning permissions.
           )")
{
  LY_ASSERT_TYPE (ly_is_symbol, symbol, 1);
  Page_marker *page_marker = new Page_marker ();
  page_marker->set_permission (symbol, permission);
  return page_marker->unprotect ();
}

LY_DEFINE (ly_make_page_label_marker, "ly:make-page-label-marker", 1, 0, 0,
           (SCM label),
           R"(
Return page marker with label @var{label}.
           )")
{
  LY_ASSERT_TYPE (ly_is_symbol, label, 1);
  Page_marker *page_marker = new Page_marker ();
  page_marker->set_label (label);
  return page_marker->unprotect ();
}
