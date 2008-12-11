/*
  libc-extension.hh -- declare some string.h extensions

  source file of the flowerlib

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
