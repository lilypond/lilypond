/*
  libc-extension.hh -- declare some string.h extensions

  source file of the flowerlib

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LIBC_EXTENSION_HH
#define LIBC_EXTENSION_HH
#include "fproto.hh"

char* strnlwr( char* start_l ,int n);
char* strnupr( char* start_l, int n);
/*
  should use void* like in libc
 */
char *memmem(const Byte * haystack, const Byte *needle,
	       int haystack_len, int needle_len);
Byte *memrchr(const Byte * p, int n, char c);
Byte*strrev( Byte* byte_l, int length_i );


#endif // LIBC_EXTENSION_HH
