/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef FILE_NAME_HH
#define FILE_NAME_HH

#include "std-string.hh"

std::string dir_name (const std::string &file_name);
std::string get_working_directory ();

// Split file name
//
//   C:/foo/bar/baz.xy
//
// as follows:
//
//   root_        = "C"
//   dir_         = "/foo/bar"
//   base_        = "baz"
//   ext_         = "xy"
//   is_absolute_ = true
//
// In other words, the trailing root, directory, and extension separators
// are removed.
//
class File_name
{
public:
  std::string root_;
  std::string dir_;
  std::string base_;
  std::string ext_;
  bool is_absolute_;

  File_name (std::string = "");

  File_name absolute (std::string const &cwd) const;
  bool is_absolute () const;
  std::string to_string () const;
  File_name canonicalized () const;
  std::string dir_part () const;
  std::string file_part () const;
};

#endif /* FILE_NAME */
