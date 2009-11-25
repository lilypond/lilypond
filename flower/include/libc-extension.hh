/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef LIBC_EXTENSION_HH
#define LIBC_EXTENSION_HH

#include <cstddef>
#include <cstdarg>
using namespace std;

#include "config.hh"

char *strnlwr (char *start, int n);
char *strnupr (char *start, int n);

#if ! HAVE_MEMMEM		/* GNU extension. */
void *memmem (void const *haystack, int haystack_len,
	      void const *needle, int needle_len);
#endif /* HAVE_MEMMEM */

#if ! HAVE_MEMRCHR
unsigned char *memrchr (unsigned char const *p, int n, char c);
#endif

#if ! HAVE_MEMREV
unsigned char *memrev (unsigned char *byte, int length_i);
#endif

double my_round (double);

/* namespace std { */
  
#if ! HAVE_SNPRINTF		/* GNU extension. */
int snprintf (char *str, size_t n, char const *format, ...);
 __attribute__ ((format (printf, 3, 4)));
#endif

#if ! HAVE_VSNPRINTF	 	/* GNU extension. */
int vsnprintf (char *str, size_t, char const *format, va_list args);
#endif

#ifndef isinf
#if ! HAVE_ISINF		/* BSD extension. */
int isinf (double x);
#endif
#endif

/* }; */

#endif /* LIBC_EXTENSION_HH */
