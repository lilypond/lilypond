/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2023 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "yaffut.hh"

#include <string>

using namespace std::string_literals;

class String_convert_test
{
};

TEST (String_convert_test, bin2hex)
{
  EQUAL (String_convert::bin2hex ('\x00'), "00"s);
  EQUAL (String_convert::bin2hex ('\x5a'), "5a"s);
  EQUAL (String_convert::bin2hex ('\xa5'), "a5"s);
  EQUAL (String_convert::bin2hex ('\xff'), "ff"s);
}

TEST (String_convert_test, hex2bin)
{
  EQUAL (String_convert::hex2bin ("005aa5ff"), "\x00\x5a\xa5\xff"s);
}

TEST (String_convert_test, percent_encode)
{
  EQUAL (String_convert::percent_encode ("A+Z=%X"), "A%2bZ%3d%25X"s);
}
