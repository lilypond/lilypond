/*
  libc-extension.hh -- declare some string.h extensions

  source file of the flowerlib

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LIBC_EXTENSION_HH
#define LIBC_EXTENSION_HH
#include "fproto.hh"
#include "config.hh"

char* strnlwr( char* start_l ,int n);
char* strnupr( char* start_l, int n);

#ifndef HAVE_MEMMEM		// GNU extension.
char *memmem(const Byte * haystack, int haystack_len,
	     const Byte *needle, int needle_len);
#endif HAVE_MEMMEM

#ifndef HAVE_SNPRINTF		// GNU extension.
int snprintf (char *str, size_t n,
	      const char *format, ... );
#endif


Byte *memrchr(const Byte * p, int n, char c);
Byte *strrev( Byte* byte_l, int length_i );


#endif // LIBC_EXTENSION_HH
