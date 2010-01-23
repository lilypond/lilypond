/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "lily-version.hh"

#include "config.hh"
#include "version.hh"

string
version_string ()
{
  string str = MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL;
  string mpl ("." MY_PATCH_LEVEL);
  if (mpl != ".")
    str += mpl;
  return str;
}

string
gnu_lilypond_string ()
{
  string str = "GNU LilyPond";
  return str;
}

string
gnu_lilypond_version_string ()
{
  string str = gnu_lilypond_string () + " " + version_string ();
  return str;
}

