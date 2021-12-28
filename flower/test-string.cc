/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>,

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

#include "std-vector.hh"

#include <iostream>

#include "yaffut.hh"

using std::string;
using std::vector;

FUNC (string_split_join)
{
  string orig = "a/bbbb/cc//d";
  vector<string> splits = string_split (orig, '/');
  string loop = string_join (splits, "/");
  EQUAL (orig, loop);
  EQUAL (splits.size (), size_t (5));
}
