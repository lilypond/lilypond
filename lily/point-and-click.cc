/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2023 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "point-and-click.hh"

#include "file-name.hh"
#include "input.hh"
#include "lily-guile.hh"
#include "string-convert.hh"

std::string
format_point_and_click_url (Stream_event *ev)
{
  SCM location = get_property (ev, "origin");
  Input *origin = unsmob<Input> (location);
  if (!origin)
    return std::string {};

  ssize_t line, chr, col, unused;
  origin->get_counts (&line, &chr, &col, &unused);

  File_name name (origin->file_string ());
  auto const file_ = String_convert::percent_encode (
    name.absolute (get_working_directory ()).to_string ());

  return String_convert::form_string ("textedit://%s:%ld:%ld:%ld",
                                      file_.c_str (), line, chr, col);
}
