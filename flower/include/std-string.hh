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
  // typedef string std::string;
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

  std::string to_string (std::string s);
  std::string to_string (char c, int n = 1);
  std::string to_string (int i, char const *format = 0);
  std::string to_string (double f, char const *format = 0);
  std::string to_string (long b);
  std::string to_string (bool b);
  std::string to_string (char const *format, ...);
  
  std::string &replace_all (std::string &str, std::string find, std::string replace);
  std::string &replace_all (std::string &str, char find, char replace);
  char *string_copy (std::string s);
  
  int string_compare (std::string const &, std::string const &);
  INSTANTIATE_COMPARE (std::string const &, string_compare);
}

#endif /* STD_STRING_HH */
