/*
  libc-extension.cc --  compensate for lacking libc functions.


  source file of the flowerlib

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include "libc-extension.hh"


char* 
strnlwr (char* start_l ,int n)
{
  char * p = start_l + n;
  while (--p >= start_l) 
    {
      *p = tolower (*p);    /* a macro on some compilers */
    }
  return start_l;
}

char* 
strnupr (char* start_l, int n)
{
  char * p = start_l + n;
  while (--p >= start_l) 
    {
      *p = toupper (*p);    /* a macro on some compilers */
    }
  return start_l;
}

#if !HAVE_MEMMEM

/** locate a substring. #memmem# finds the first occurrence of
  #needle# in #haystack#.  This is not ANSI-C.

  The prototype is not in accordance with the Linux Programmer's
  Manual v1.15, but it is with /usr/include/string.h   */

Byte *
memmem (Byte const *needle,int needle_len,
	Byte const *haystack, int haystack_len)
{
  Byte const * end_haystack = haystack + haystack_len - needle_len + 1;
  Byte const * end_needle = needle + needle_len ;

  /* Ahhh ... Some minimal lowlevel stuff. This *is* nice; Varation
     is the spice of life */
  while (haystack < end_haystack) 
    {
      Byte const *subneedle_l = needle;
      Byte const *subhaystack_l = haystack;
      while (subneedle_l < end_needle) 
	{
	  if (*subneedle_l++ != *subhaystack_l++)
	    {
	      haystack ++;
	      continue;
	    }
	}
	
      // completed the needle. Gotcha.
      return (Byte *) haystack;
    }
  return 0;
}

#endif

Byte *
memrchr (Byte const * p, int n, char c)
{
  const    Byte * q = p+n;
  while (q > p) 
    {
      if (*--q == c)
	return (Byte*)q;
    }
  return 0;
}


template<class T>
inline void
my_swap (T &t1, T &t2, T &tmp)
{
  tmp = t1;
  t1 = t2;
  t2 = tmp;
}

Byte*
strrev (Byte* byte_l, int length_i)
{
  Byte tmp_byte;
  
  Byte* left_l = byte_l;
  Byte* right_l = byte_l + length_i;

  while (right_l > left_l) 
    {
      my_swap (*right_l-- , *left_l++ , tmp_byte);
    }
  return byte_l;
}

#if ! HAVE_SNPRINTF
int snprintf (char *str, size_t,
	      char const *format, ...)
{
  va_list ap;
  va_start (ap, format);
  int i = vsprintf (str, format, ap);
  va_end (ap);
  return i;
}
#endif
