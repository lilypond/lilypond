/*
  std-tring.cc -- implement external interface for Std_String

  source file of the GNU LilyPond music typesetter

  (c) 2006  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "std-string.hh"

#if STD_STRING

namespace std {
  Std_string
  to_std_string (char c, int n)
  {
    /* FIXME, remove this function and use std::string interface for
       String?  This interface is a bit clumsy, almost alway you want
       n=1.  */
    return Std_string (n, c);
  }

#define FIND_FAILED string::npos
#define SIZE_T size_t
#else /* !STD_STRING */

#define FIND_FAILED -1
#define SIZE_T int

#endif /* STD_STRING */

Std_string &
replace_all (Std_string &str, Std_string find, Std_string replace)
{
  int len = find.length ();
  for (SIZE_T i = str.find (find); i != FIND_FAILED; i = str.find (find,
								   i + len))
    str = str.replace (i, len, replace);
  return str;
}

#if STD_STRING
}
#endif
