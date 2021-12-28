/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "international.hh"
#include "string-convert.hh"

#if !HAVE_GETTEXT
inline char *
gettext (char const *s)
{
  return const_cast<char *> (s);
}
#else
#include <libintl.h>
#endif

using std::string;

string
_ (char const *ch)
{
  return string (gettext (ch));
}

string
_f (char const *format, ...)
{
  va_list args;
  va_start (args, format);
  string str = v_f (format, args);
  va_end (args);
  return str;
}

string
v_f (char const *format, va_list args)
{
  return String_convert::vform_string (gettext (format), args);
}

string
_f (char const *format, const string &s, const string &s2, const string &s3)
{
  return String_convert::form_string (gettext (format), s.c_str (), s2.c_str (),
                                      s3.c_str ());
}
