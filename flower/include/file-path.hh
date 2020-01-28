/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef FILE_PATH_HH
#define FILE_PATH_HH

#include "std-string.hh"
#include "std-vector.hh"

/**
   search in directories for a file.

   Abstraction of PATH variable. An interface for searching input files.
   Search a number of dirs for a file.

   TODO: add a unix style PATH interface
*/

class File_path
{
  std::vector<std::string> dirs_;

public:
  std::vector<std::string> directories () const;
  std::string find (const std::string &name) const;
  std::string find (const std::string &name, char const *extensions[]);
  std::string to_string () const;
  bool try_append (std::string str);
  void append (const std::string &);
  void parse_path (const std::string &);
  void prepend (const std::string &);
};

bool is_file (const std::string &file_name);
bool is_dir (std::string file_name);

#endif /* FILE_PATH */
