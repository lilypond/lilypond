/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef STD_STRING_HH
#define STD_STRING_HH

#include "unistd.h"

#if 0
/*
  leads to dubious crashes - libstdc++  bug?
 */
#ifdef DEBUG
#define _GLIBCXX_DEBUG 1
#endif
#endif

#include <string>

typedef size_t ssize;
#define NPOS std::string::npos

// TODO: This should probably be renamed to avoid any possible confusion with
// std::to_string.
std::string to_string (char const *format, ...)
  __attribute__ ((format (printf, 1, 2)));

std::string &replace_all (std::string *str, std::string const &find,
                          std::string const &replace);
std::string &replace_all (std::string *str, char find, char replace);

#endif /* STD_STRING_HH */
