/*
  std-tring.cc -- implement external interface for Std_String

  source file of the GNU LilyPond music typesetter

  (c) 2006  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#if STD_STRING
#include "std-string.hh"

namespace std {
  Std_string to_std_string (char c, int n)
  {
    /* FIXME, remove this function and use std::string interface for
       String?  This interface is a bit clumsy, almost alway you want
       n=1.  */
    return Std_string (n, c);
  }
}

#endif /* STD_STRING */
