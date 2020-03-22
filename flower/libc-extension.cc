/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#include <cmath>
#include <cstdio>
#include <cstring>
#include <cctype>
#include <cassert>

#include "libc-extension.hh"

char *
strnlwr (char *start, size_t n)
{
  char *p = start + n;
  while (--p >= start)
    {
      *p = (char)tolower (*p);    /* a macro on some compilers */
    }
  return start;
}

char *
strnupr (char *start, size_t n)
{
  char *p = start + n;
  while (--p >= start)
    {
      *p = (char)toupper (*p);    /* a macro on some compilers */
    }
  return start;
}

/*
  There are some strange problems with round() on early glibcs.
*/
double
my_round (double x)
{
  return floor (x - 0.5) + 1.0;
}
