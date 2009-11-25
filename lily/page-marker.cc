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

#include "page-marker.hh"
#include "ly-smobs.icc"

Page_marker::Page_marker ()
{
  symbol_ = SCM_EOL;
  permission_ = SCM_EOL;
  label_ = SCM_EOL;
  smobify_self ();
}

Page_marker::~Page_marker ()
{
}

IMPLEMENT_SMOBS (Page_marker);
IMPLEMENT_DEFAULT_EQUAL_P (Page_marker);
IMPLEMENT_TYPE_P (Page_marker, "ly:page-marker?");

SCM
Page_marker::mark_smob (SCM smob)
{
  Page_marker *pm = (Page_marker *) SCM_CELL_WORD_1 (smob);
  scm_gc_mark (pm->symbol_);
  scm_gc_mark (pm->permission_);
  scm_gc_mark (pm->label_);
  return SCM_EOL;
}

int
Page_marker::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Page_marker *pm = (Page_marker *) SCM_CELL_WORD_1 (smob);
  (void)pm;
  scm_puts ("#<Page_marker>", port);
  return 1;
}

SCM
Page_marker::permission_symbol ()
{
  return symbol_;
}

SCM
Page_marker::permission_value ()
{
  return permission_;
}

SCM
Page_marker::label ()
{
  return label_;
}

void
Page_marker::set_permission (SCM symbol, SCM permission)
{
  symbol_ = symbol;
  permission_ = permission;
}

void
Page_marker::set_label (SCM label)
{
  label_ = label;
}


