/*
  std-string.hh -- declare std::string

  source file of the GNU LilyPond music typesetter

  (c) 2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef STD_STRING_HH
#define STD_STRING_HH

#if !STD_STRING

/* Also declare string, in the wrong way.  */
#include <algorithm>
#include <iostream>
#include <sstream>

#endif


#include "compare.hh"

#if STD_STRING
#include <string>
#endif

#if STD_STRING

namespace std {

  typedef size_t ssize;
#define NPOS std::string::npos

}

#else /* ! STD_STRING */

namespace std {

#define string String
  using namespace std;
  class String;
  typedef int ssize;
#define NPOS -1

}

#include "string.hh"

#endif /* STD_STRING */

namespace std {

  string to_string (string s);
  string to_string (char c, int n = 1);
  string to_string (int i, char const *format = 0);
  string to_string (double f, char const *format = 0);
  string to_string (long b);
  string to_string (bool b);
  string to_string (char const *format, ...);
  
  string &replace_all (string &str, string find, string replace);
  string &replace_all (string &str, char find, char replace);
  char *string_copy (string s);
  
  int string_compare (string const &, string const &);

  INSTANTIATE_COMPARE (string const &, string_compare);
}


#endif /* STD_STRING_HH */
