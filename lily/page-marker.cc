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

Page_marker::Page_marker ()
{
  symbol_ = SCM_EOL;
  permission_ = SCM_EOL;
  label_ = SCM_EOL;
  smobify_self ();
}

Page_marker::Page_marker (Page_marker const &src)
  : Smob<Page_marker> ()
{
  symbol_ = src.symbol_;
  permission_ = src.permission_;
  label_ = src.label_;
  smobify_self ();
}

Page_marker::~Page_marker ()
{
}

const char *const Page_marker::type_p_name_ = "ly:page-marker?";

SCM
Page_marker::mark_smob () const
{
  scm_gc_mark (symbol_);
  scm_gc_mark (permission_);
  scm_gc_mark (label_);
  return SCM_EOL;
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
