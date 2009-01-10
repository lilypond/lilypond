/*
  international.hh -- declare stuff for internationalization

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef INTERNATIONAL_HH
#define INTERNATIONAL_HH

#include <stdarg.h>

#include "std-string.hh"

/**
   Internationalisation: _i ("to be translated") gets an entry in the POT file
   gettext () must be invoked explicitely to do the actual "translation".
   See flower/getopt-long.cc.
*/
#define _i(sz) sz

// don't inline: get warnings only once
/**
   Internationalisation: _ ("to be translated") gets "translated" by GNU gettext
*/
string _ (char const *ch);

/**
   Internationalisation: _f ("Usage: %s [FILE]", "lilypond") gets "translated" by
   GNU gettext
*/
string _f (char const *format, ...)
  	   __attribute__ ((format (printf, 1, 2)));
string _f (char const *format, string s, string s2 = "", string s3 = "");
/**
   va_list version of _f
 */
string v_f (char const *format, va_list args);

#endif // INTERNATIONAL_HH

