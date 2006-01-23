/*
  std-string.hh -- declare Std_string

  source file of the GNU LilyPond music typesetter

  (c) 2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef STD_STRING_HH
#define STD_STRING_HH

#if !STD_STRING

#define Std_string String
#define to_std_string to_string
class String;
#include "string.hh"

#else

#include <string>
// #warning Using std::string

namespace std {

#if 0
  class Std_string : public string
  {
  public:
    Std_string ();
    Std_string (char const*);
    Std_string (Std_string const&, int pos, int n=npos);
    ///Std_string (String const&, int pos, int n);
    ////Std_string (String const &);
    ////operator String ();
  };
#else  
  typedef string Std_string;
#endif

  //operator Std_string (String const&);

  Std_string to_std_string (Std_string s);
  Std_string to_std_string (char c, int n = 1);
  Std_string to_std_string (int i, char const *format = 0);
  Std_string to_std_string (double f, char const *format = 0);
  Std_string to_std_string (long b);
  Std_string to_std_string (bool b);
  Std_string to_std_string (char const *format, ...);

#endif /* STD_STRING */

  Std_string &replace_all (Std_string &str, Std_string find, Std_string replace);
#if STD_STRING
}
#endif
  

#endif /* STD_STRING_HH */
