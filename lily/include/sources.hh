/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef SOURCES_HH
#define SOURCES_HH

#include "lily-proto.hh"
#include "std-vector.hh"

class Sources
{
  Sources (Sources const &);
  vector<Source_file*> sourcefiles_;

public:
  Sources ();
  ~Sources ();

  Source_file *get_file (string file_name, string const& currentpath);
  void add (Source_file *sourcefile);
  void set_path (File_path *);

  const File_path *path_;
};

#endif /* SOURCE_HH */
