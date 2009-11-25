/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2009 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef STD_STRING_HH
#define STD_STRING_HH
 
#include "compare.hh"
#include "flower-proto.hh"

#if 0
/*
  leads to dubious crashes - libstdc++  bug?
 */
#ifndef NDEBUG
#define _GLIBCXX_DEBUG 1
#endif
#endif

#include <string>

using namespace std;

typedef size_t ssize;
#define NPOS string::npos

string to_string (string s);
string to_string (char c, int n=1);
string to_string (int i, char const *format=0);
string to_string (double f, char const *format=0);
string to_string (long);
string to_string (long unsigned);
string to_string (I64, char const *format=0);
string to_string (unsigned);
string to_string (bool b);
string to_string (char const *format, ...)
  __attribute__ ((format (printf, 1, 2)));
  
string &replace_all (string* str, string const &find, string const &replace);
string &replace_all (string* str, char find, char replace);
char *string_copy (string s);

int string_compare (string const &, string const &);

INSTANTIATE_COMPARE (string const &, string_compare);


#endif /* STD_STRING_HH */
