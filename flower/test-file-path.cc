/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "config.hh"

#include "file-path.hh"
#include "file-name.hh"

#include <limits.h>
#include <unistd.h>

#include "yaffut.hh"

using std::string;

TEST (File_path, Find)
{
  char const *extensions[] = {"ly", "", 0};
  string file = "init";
  if (get_working_directory ().empty ())
    {
      FAIL ("Could not get current work directory\n");
    }
  char *top_src_dir = getenv ("top-src-dir");
  if (!top_src_dir)
    {
      FAIL ("Could not get top source directory\n");
    }
  string ly_dir = string (top_src_dir) + "/ly";
  parse_path (string (1, PATHSEP) + ly_dir);
  string file_name = find (file, extensions);
  EQUAL (file_name.substr (file_name.rfind ('/')), "/init.ly");
  file = "init.ly";
  file_name = find (file, extensions);
  EQUAL (file_name, ly_dir + "/init.ly");
}
