/*   
  international.cc -- implement stuff for internationalisation
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include <stdarg.h>
#include "config.h"
#include "string-convert.hh"
#include "international.hh"

#if !HAVE_GETTEXT
inline char*
gettext (char const* s)
{
  return (char*)s;
}
#else
#include <libintl.h>
#endif

String 
_ (char const *ch)
{
  return String (gettext (ch));
}

String 
_f (char const* format, ...)
{
  va_list args;
  va_start (args, format);
  String str = String_convert::vform_str (gettext (format), args);
  va_end (args);
  return str;
}

String 
_f (char const* format, String s, String s2, String s3)
{
  return String_convert::form_str (gettext (format), s.ch_C (), s2.ch_C (), 
    s3.ch_C ());
}
