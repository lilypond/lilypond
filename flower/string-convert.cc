/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>,
                           Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "string-convert.hh"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <string>

using std::string;

/**
   A safe length for stringconversion buffers.

   worst case would be %f printing HUGE (or 1/HUGE), which is approx
   2e318, this number would have approx 318 zero's in its string.

   Should enlarge buff dynamically.

   @see
   man 3 snprintf
*/
static const int STRING_BUFFER_LEN = 1024;

static char
nibble2hex (int byte)
{
  const auto nibble = byte & 0x0f;
  if (nibble <= 9)
    return static_cast<char> ('0' + nibble);
  else
    return static_cast<char> ('a' + (nibble - 10));
}

string
String_convert::bin2hex (Byte bin_char)
{
  string str;
  str += nibble2hex (bin_char >> 4);
  str += nibble2hex (bin_char);
  return str;
}

int
String_convert::hex2bin (string hex_string, string &bin_string_r)
{
  if (hex_string.length () % 2)
    hex_string = "0" + hex_string;

  bin_string_r = "";
  for (auto it = hex_string.begin (); it != hex_string.end (); /*in loop*/)
    {
      int high_i = hex2nibble (*it++);
      int low_i = hex2nibble (*it++);
      if (high_i < 0 || low_i < 0)
        return 1; // invalid char
      bin_string_r += static_cast<char> (high_i << 4 | low_i);
    }
  return 0;
}

string
String_convert::hex2bin (const string &hex_string)
{
  string str;

  if (hex2bin (hex_string, str))
    assert (false);

  return str;
}

int
String_convert::hex2nibble (char byte)
{
  if (byte >= '0' && byte <= '9')
    return byte - '0';
  if (byte >= 'A' && byte <= 'F')
    return byte - 'A' + 10;
  if (byte >= 'a' && byte <= 'f')
    return byte - 'a' + 10;
  return -1;
}

string
String_convert::form_string (char const *format, ...)
{
  va_list args;
  va_start (args, format);
  char buffer[STRING_BUFFER_LEN];
  vsnprintf (buffer, STRING_BUFFER_LEN, format, args);
  va_end (args);
  return string (buffer);
}

string
String_convert::vform_string (char const *format, va_list args)
{
  char buffer[STRING_BUFFER_LEN];
  vsnprintf (buffer, STRING_BUFFER_LEN, format, args);
  return string (buffer);
}

string
String_convert::pad_to (const string &s, size_t n)
{
  size_t length = s.length ();
  if (n <= length)
    return s;
  return s + string (n - length, ' ');
}

string
String_convert::to_upper (string s)
{
  // Implicit copy of argument, no reference.
  std::transform (s.begin (), s.end (), s.begin (), ::toupper);
  return s;
}

string
String_convert::to_lower (string s)
{
  // Implicit copy of argument, no reference.
  std::transform (s.begin (), s.end (), s.begin (), ::tolower);
  return s;
}

string
String_convert::be_u16 (uint16_t u)
{
  uint8_t r[2];
  r[0] = uint8_t (u >> 8);
  r[1] = uint8_t (u);
  return string (reinterpret_cast<char *> (r), 2);
}

string
String_convert::be_u32 (uint32_t u)
{
  uint8_t r[4];
  r[0] = uint8_t (u >> 24);
  r[1] = uint8_t (u >> 16);
  r[2] = uint8_t (u >> 8);
  r[3] = uint8_t (u);
  return string (reinterpret_cast<char *> (r), 4);
}

string
String_convert::be_u24 (uint32_t u)
{
  uint8_t r[3];
  r[0] = uint8_t (u >> 16);
  r[1] = uint8_t (u >> 8);
  r[2] = uint8_t (u);
  return string (reinterpret_cast<char *> (r), 3);
}

static bool
is_not_escape_character (Byte c)
{
  if (('a' <= c) && (c <= 'z'))
    return true;

  if (('A' <= c) && (c <= 'Z'))
    return true;

  if (('0' <= c) && (c <= '9'))
    return true;

  switch (c)
    {
    case '-':
    case '.':
    case '/':
    case ':':
    case '_':
      return true;
    }

  return false;
}

string
String_convert::percent_encode (const string &orig_str)
{
  string new_str;
  vsize i = 0;
  vsize n = orig_str.size ();

  while (i < n)
    {
      Byte cur = orig_str[i];

      if (is_not_escape_character (cur))
        new_str += cur;
      else
        {
          new_str += '%';
          new_str += String_convert::bin2hex (cur);
        }

      i++;
    }
  return new_str;
}
