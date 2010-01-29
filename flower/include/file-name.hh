/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "std-vector.hh"
#include "std-string.hh"

std::string dir_name (std::string const file_name);
std::string get_working_directory ();

class File_name
{
public:
  string root_;
  string dir_;
  string base_;
  string ext_;

  File_name (string="");

  bool is_absolute () const;
  string to_string () const;
  File_name canonicalized () const;
  string dir_part () const;
  string file_part () const;
};

#endif /* FILE_NAME */
