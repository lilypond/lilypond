/*
  libc-extension.hh -- declare some string.h extensions

  source file of the flowerlib

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef LIBC_EXTENSION_HH
#define LIBC_EXTENSION_HH

#include <cstddef>
#include <cstdarg>

#include "config.hh"
#include "flower-proto.hh"

char* strnlwr (char* start ,int n);
char* strnupr (char* start, int n);

#if !HAVE_MEMMEM		// GNU extension.
void *memmem (void const * haystack, int haystack_len,
	     void const *needle, int needle_len);
#endif /* HAVE_MEMMEM */

#if !HAVE_SNPRINTF		// GNU extension.
int snprintf (char *str, size_t n, char const *format, ...);
#endif

#if !HAVE_VSNPRINTF	 	// GNU extension.
int vsnprintf (char *str, size_t, char const *format, va_list args);
#endif

#ifndef isinf
#if !HAVE_ISINF			// BSD extension 
int isinf (double x);
#endif
#endif


Byte *memrchr (Byte const * p, int n, char c);
Byte *strrev (Byte* byte, int length_i);


double my_round (double);

#endif // LIBC_EXTENSION_HH
