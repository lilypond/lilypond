/*   
  international.hh -- declare stuff for internationalization
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef INTERNATIONAL_HH
#define INTERNATIONAL_HH

#include "string.hh"

// don't inline: get warnings only once
/**
 Internationalisation: _ ("to be translated") gets "translated" by GNU gettext
*/
String _ (char const *ch);

/**
 Internationalisation: _f ("Usage: %s [FILE]", "lilypond") gets "translated" by 
 GNU gettext
*/
String _f (char const* format, ...);
String _f (char const* format, String s, String s2 = "", String s3 = "");

#endif // INTERNATIONAL_HH

