/*
  libc-extension.hh -- declare some string.h extensions

  source file of the flowerlib

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef LIBC_EXTENSION_HH
#define LIBC_EXTENSION_HH

#include "fproto.hh"
#include "config.h"
#include <cstddef>
#include <stdarg.h>

char* strnlwr (char* start_l ,int n);
char* strnupr (char* start_l, int n);

#if !HAVE_MEMMEM		// GNU extension.
Byte *memmem (Byte const * haystack, int haystack_len,
	     Byte const *needle, int needle_len);
#endif HAVE_MEMMEM

#if !HAVE_SNPRINTF		// GNU extension.
int snprintf (char *str, size_t n, char const *format, ...);
#endif

#if !HAVE_VSNPRINTF	 	// GNU extension.
int vsnprintf (char *str, size_t, char const *format, va_list args);
#endif


Byte *memrchr (Byte const * p, int n, char c);
Byte *strrev (Byte* byte_l, int length_i);


#endif // LIBC_EXTENSION_HH
