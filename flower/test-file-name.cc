/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "file-name.hh"

#include "yaffut-parameters.hh"

using std::string;

string slashify (string file_name);

struct CaseB : public yaffut::Test<File_name, CaseB>
{
  CaseB ()
    : File_name ("123")
  {
  }
};

TEST_STRING (File_name, Mingw_slashify, "foe.ly")
{
  string s = slashify (to_string ());
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Mingw_slashify_2, "/tmp/x.ly")
{
  string s = slashify (to_string ());
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Mingw_slashify_3, "c:/tmp/x.ly")
{
  string s = slashify (to_string ());
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Mingw_slashify_4, "\\tmp\\x.ly")
{
  string s = slashify (to_string ());
  EQUAL ("/tmp/x.ly", s);
}

TEST_STRING (File_name, Canonicalize_1, "foo//bar/./..//bla//z.ly")
{
  string s = canonicalized ().to_string ();
  EQUAL ("foo/bla/z.ly", s);
}

TEST_STRING (File_name, Canonicalize_2, "./foo")
{
  string s = canonicalized ().to_string ();
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Canonicalize_3, "/")
{
  string s = canonicalized ().to_string ();
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Canonicalize_4, "/foo")
{
  string s = canonicalized ().to_string ();
  EQUAL (parameter_one_, s);
}

TEST_STRING (File_name, Canonicalize_5, "foo/")
{
  string s = canonicalized ().to_string ();
  EQUAL ("foo", s);
}

TEST_STRING (File_name, Canonicalize_6, "foo/./..")
{
  string s = canonicalized ().to_string ();
  EQUAL (".", s);
}

TEST_STRING (File_name, Canonicalize_7, "../.")
{
  string s = canonicalized ().to_string ();
  EQUAL ("..", s);
}

TEST_STRING (File_name, Canonicalize_8, "./../")
{
  string s = canonicalized ().to_string ();
  EQUAL ("..", s);
}

TEST (File_name, absolute)
{
  EQUAL (File_name ("abc/def.g").absolute ("/home").to_string (),
         "/home/abc/def.g");

  EQUAL (File_name ("c:/abc/def.g").absolute ("/home").to_string (),
         "c:/abc/def.g");
  EQUAL (File_name ("/abc/def.g").absolute ("/home").to_string (),
         "/abc/def.g");
  EQUAL (File_name ("/def.g").absolute ("/home").to_string (), "/def.g");
  EQUAL (File_name ("def.g").absolute ("/home").to_string (), "/home/def.g");
  EQUAL (File_name ("c:abc/def.g").absolute ("c:/home").to_string (),
         "c:/home/abc/def.g");
  EQUAL (File_name ("d:abc/def.g").absolute ("c:/home").to_string (),
         "c:/home/abc/def.g");
}
