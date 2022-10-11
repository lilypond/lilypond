/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef INTERNATIONAL_HH
#define INTERNATIONAL_HH

#include <stdarg.h>

#include "std-string.hh"

/**
   Internationalisation: _i ("to be translated") gets an entry in the POT file
   gettext () must be invoked explicitly to do the actual "translation".
   See flower/getopt-long.cc.
*/
#define _i(sz) sz

// don't inline: get warnings only once
/**
   Internationalisation: _ ("to be translated") gets "translated" by GNU gettext
*/
std::string _ (char const *ch);

/**
   Internationalisation: _f ("Usage: %s [FILE]", "lilypond") gets "translated" by
   GNU gettext
*/
std::string _f (char const *format, ...)
  __attribute__ ((format (printf, 1, 2)));
std::string _f (char const *format, const std::string &s,
                const std::string &s2 = "", const std::string &s3 = "");
/**
   va_list version of _f
 */
std::string v_f (char const *format, va_list args);

#endif // INTERNATIONAL_HH
