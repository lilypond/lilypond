/*
  libc-extension.cc --  compensate for lacking libc functions.


  source file of the flowerlib

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
         Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "libc-extension.hh"

/*
  urg: why soo wierd?
 */
char* 
strnlwr (char* start ,int n)
{
  char * p = start + n;
  while (--p >= start) 
    {
      *p = tolower (*p);    /* a macro on some compilers */
    }
  return start;
}

char* 
strnupr (char* start, int n)
{
  char * p = start + n;
  while (--p >= start) 
    {
      *p = toupper (*p);    /* a macro on some compilers */
    }
  return start;
}


#if !HAVE_MEMMEM

/** locate a substring. #memmem# finds the first occurrence of
  #needle# in #haystack#.  This is not ANSI-C.

  The prototype is not in accordance with the Linux Programmer's
  Manual v1.15, but it is with /usr/include/string.h   */

Byte *
_memmem (Byte const *haystack, int haystack_len,
	Byte const *needle,int needle_len)
{
  Byte const * end_haystack = haystack + haystack_len - needle_len + 1;
  Byte const * end_needle = needle + needle_len ;

  /* Ahhh ... Some minimal lowlevel stuff. This *is* nice; Varation
     is the spice of life */
  while (haystack < end_haystack) 
    {
      Byte const *subneedle = needle;
      Byte const *subhaystack = haystack;
      while (subneedle < end_needle) 
        if (*subneedle++ != *subhaystack++)
	  goto next;
	
      // completed the needle. Gotcha.
      return (Byte *) haystack;
      next:
	haystack++;
    }
  return 0;
}

void *
memmem (void const *haystack, int haystack_len,
	void const *needle,int needle_len)
{
  Byte const* haystack_byte_c = (Byte const*)haystack;
  Byte const* needle_byte_c = (Byte const*)needle;
  return _memmem (haystack_byte_c, haystack_len, needle_byte_c, needle_len);
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
strrev (Byte* byte, int length_i)
{
  Byte tmp_byte;
  
  Byte* left = byte;
  Byte* right = byte + length_i;

  while (right > left) 
    {
      my_swap (*right-- , *left++ , tmp_byte);
    }
  return byte;
}

#if ! HAVE_SNPRINTF
int 
snprintf (char *str, size_t, char const *format, ...)
{
  va_list ap;
  va_start (ap, format);
  int i = vsprintf (str, format, ap);
  va_end (ap);
  return i;
}
#endif

#if ! HAVE_VSNPRINTF
int 
vsnprintf (char *str, size_t, char const *format, va_list args)
{
  int i = vsprintf (str, format, args);
  return i;
}
#endif


#if !HAVE_ISINF
int
isinf (double x)
{
  return x && ( x == x/ 2) ;
}

#endif
