/*
  std-string.cc -- implement external interface for Std_String

  source file of the GNU LilyPond music typesetter

  (c) 2006  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "std-string.hh"
#include "string-convert.hh"

#if STD_STRING
namespace std {
#endif
  
  std::string
  to_string (std::string s)
  {
    return s;
  }

  std::string
  to_string (char c, int n)
  {
    return std::string (n, c);
  }

  std::string
  to_string (double f, char const *format)
  {
    return String_convert::double_string (f, format);
  }

  std::string
  to_string (int i, char const *format)
  {
    return String_convert::int_string (i, format);
  }

  std::string
  to_string (bool b)
  {
    return String_convert::bool_string (b);
  }

  std::string
  to_string (long b)
  {
    return String_convert::long_string (b);
  }

  std::string
  to_string (char const *format, ...)
  {
    va_list args;
    va_start (args, format);
    std::string str = String_convert::vform_string (format, args);
    va_end (args);
    return str;
  }

  std::string &
  replace_all (std::string &str, std::string find, std::string replace)
  {
    ssize len = find.length ();
    for (ssize i = str.find (find); i != NPOS; i = str.find (find, i + len))
      str = str.replace (i, len, replace);
    return str;
  }

  std::string &
  replace_all (std::string &str, char find, char replace)
  {
    for (ssize i = str.find (find); i != NPOS; i = str.find (find, i + 1))
      str[i] = replace;
    return str;
  }

  char *
  string_copy (std::string s)
  {
    ssize len = s.length ();
    char *dest = new char[len + 1];
    //s.copy (dest, len + 1);
    memcpy (dest, s.c_str (), len + 1);
    return dest;
  }

  int
  string_compare (std::string const &a, std::string const &b)
  {
    return a.compare (b);
  }

#if STD_STRING
}

#endif
