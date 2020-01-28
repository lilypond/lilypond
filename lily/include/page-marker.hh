/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2020 Nicolas Sceaux <nicolas.sceaux@free.fr>

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
#include "virtual-methods.hh"

class Page_marker : public Smob<Page_marker>
{
public:
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Page_marker ();

private:
  SCM symbol_;     /* either 'page-turn-permission or 'page-break-permission */
  SCM permission_; /* 'force, 'allow, or '() */
  SCM label_;      /* bookmarking label (a symbol) */

public:
  Page_marker ();
  Page_marker (Page_marker const &);
  VIRTUAL_CLASS_NAME (Page_marker);
  virtual Page_marker *clone () const { return new Page_marker (*this); }

  void set_permission (SCM symbol, SCM permission);
  void set_label (SCM label);

  SCM permission_symbol ();
  SCM permission_value ();
  SCM label ();
};

#endif /* PAGE_MARKER_HH */
