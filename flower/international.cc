/*
  international.cc -- implement stuff for internationalisation

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "config.hh"

#include "international.hh"
#include "string-convert.hh"

#if !HAVE_GETTEXT
inline char *
gettext (char const *s)
{
  return (char *)s;
}
#else
#include <libintl.h>
#endif

string
_ (char const *ch)
{
  return string (gettext (ch));
}

string
_f (char const *format, ...)
{
  va_list args;
  va_start (args, format);
  string str = v_f (format, args);
  va_end (args);
  return str;
}

string
v_f (char const *format, va_list args)
{
  return String_convert::vform_string (gettext (format), args);
}

string
_f (char const *format, string s, string s2, string s3)
{
  return String_convert::form_string (gettext (format), s.c_str (), s2.c_str (),
				      s3.c_str ());
}
