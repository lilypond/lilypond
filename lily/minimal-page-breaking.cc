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

#include "international.hh"
#include "minimal-page-breaking.hh"
#include "output-def.hh"
#include "page-spacing.hh"
#include "paper-book.hh"

Minimal_page_breaking::Minimal_page_breaking (Paper_book *pb)
  : Page_breaking (pb, 0, 0)
{
}

Minimal_page_breaking::~Minimal_page_breaking ()
{
}

SCM
Minimal_page_breaking::solve ()
{
  vsize end = last_break_position ();

  message (_ ("Calculating line breaks..."));
  set_to_ideal_line_configuration (0, end);
  break_into_pieces (0, end, current_configuration (0));

  message (_ ("Calculating page breaks..."));
  int first_page_num
    = from_scm (book_->paper ()->c_variable ("first-page-number"), 1);
  Page_spacing_result res = pack_systems_on_least_pages (0, first_page_num);
  SCM lines = systems ();
  return make_pages (res.systems_per_page_, lines);
}
