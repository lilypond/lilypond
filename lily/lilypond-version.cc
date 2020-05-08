/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "lilypond-version.hh"
#include "string-convert.hh"
#include "misc.hh"

#include <ctype.h>

using std::string;
using std::vector;

Lilypond_version::Lilypond_version (int major, int minor, int patch)
{
  major_ = major;
  minor_ = minor;
  patch_ = patch;
}

Lilypond_version::Lilypond_version (const string &str)
{
  major_ = -1;
  minor_ = 0;
  patch_ = 0;

  vector<string> version;
  const char *digits = "0123456789";
  version = string_split (str, '.');

  switch (version.size ())
    {
    case 4:
      extra_patch_string_ = version[3];
      if (version[2].empty ())
        return;
    // fallthrough
    case 3:
      if (version[2].find_first_not_of (digits) != string::npos
          || version[1].empty ())
        return;
      patch_ = String_convert::dec2int (version[2]);
    // fallthrough
    case 2:
      if (version[1].find_first_not_of (digits) != string::npos
          || version[1].empty () || version[0].empty ())
        return;
      minor_ = String_convert::dec2int (version[1]);
      if (version[0].find_first_not_of (digits) != string::npos)
        return;
      major_ = String_convert::dec2int (version[0]);
    }
}

string
Lilypond_version::to_string () const
{
  if (major_ < 0)
    return "invalid";
  return std::to_string (major_)
         + "." + std::to_string (minor_)
         + "." + std::to_string (patch_);
}

Lilypond_version::operator bool () const
{
  return !(major_ < 0);
}

int
Lilypond_version::compare (const Lilypond_version &a, const Lilypond_version &b)
{
  if (!a || !b)
    return 0;
  if (a.major_ != b.major_)
    return a.major_ > b.major_ ? 1 : -1;
  if (a.minor_ != b.minor_)
    return a.minor_ > b.minor_ ? 1 : -1;
  if (a.patch_ != b.patch_)
    return a.patch_ > b.patch_ ? 1 : -1;
  return 0;
}
