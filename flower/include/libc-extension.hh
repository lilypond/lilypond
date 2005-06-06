/*
  libc-extension.hh -- declare some string.h extensions

  source file of the flowerlib

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef LIBC_EXTENSION_HH
#define LIBC_EXTENSION_HH

#include <stddef.h>
#include <stdarg.h>

#include "config.hh"

char *strnlwr (char *start, int n);
char *strnupr (char *start, int n);

#if ! HAVE_MEMMEM		/* GNU extension. */
void *memmem (void const *haystack, int haystack_len,
	      void const *needle, int needle_len);
#endif /* HAVE_MEMMEM */

#if ! HAVE_SNPRINTF		/* GNU extension. */
int snprintf (char *str, size_t n, char const *format, ...);
#endif

#if ! HAVE_VSNPRINTF	 	/* GNU extension. */
int vsnprintf (char *str, size_t, char const *format, va_list args);
#endif

#ifndef isinf
#if ! HAVE_ISINF		/* BSD extension. */
int isinf (double x);
#endif
#endif


#if ! HAVE_MEMRCHR
unsigned char *memrchr (unsigned char const *p, int n, char c);
#endif

#if ! HAVE_MEMREV
unsigned char *memrev (unsigned char *byte, int length_i);
#endif

double my_round (double);

#endif /* LIBC_EXTENSION_HH */
