/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "lily-version.hh"

#include "international.hh"
#include "lily-guile.hh"
#include "program-option.hh"
#include "version.hh"

using std::string;

string
version_string ()
{
  if (get_program_option ("deterministic"))
    return "0.0.0";

  string str = MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL;
  string mpl ("." MY_PATCH_LEVEL);
  if (mpl != ".")
    str += mpl;
  return str;
}
