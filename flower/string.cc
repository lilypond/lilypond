/*

 string.cc - implement String
 
 (c) 1997 Han-Wen Nienhuys & Jan Nieuwenhuizen

 */

#ifndef _GNU_SOURCE // we want memmem
#define _GNU_SOURCE
#endif

#include <stdlib.h>
#include <stdio.h>

#include <assert.h>
#include <string.h>

#include "string.hh"
#include "libc-extension.hh"
#include "string-convert.hh"

#ifdef STRING_DEBUG
void* mymemmove (void* dest, void const* src, size_t n);
#define memmove mymemmove
#endif

// return array, alloced with new.
Byte*
String::copy_byte_p() const
{
  Byte const* src = strh_.byte_C();
  Byte* dest = new Byte[strh_.length_i() + 1];
  memcpy (dest, src, strh_.length_i() + 1);
  return dest;    
}
void
String::print_on (ostream& os) const
{
  if (!strh_.is_binary_bo())
    os << ch_C();
  else
    for (int i = 0; i < length_i(); i++)
      os << (Byte)(*this)[ i ];
}

/*
  copying, constructing.
 */
String&
String::operator = (String const&source)
{
  strh_ = source.strh_;
  return *this;
}

String::String (double f, char const* fmt)
{
  *this= String_convert::double_str (f,fmt);
}

String::String (char c,  int n)
{
  *this = String_convert::char_str (c,n);
}

/**
  @see
  String_convert::int_str
 */
String::String (int i, char const * format)
{
  *this = String_convert::int_str (i,format);
}

String::String (bool b)
{
  *this = (char const*) (b ? "true" : "false");
}

String::String (char const* source)
{   
  assert (source);    
  strh_ = source;    
}

String::String (Byte const* byte_l, int length_i)
{   
  strh_.set (byte_l, length_i);    
}

void
String::append (String s)
{
  strh_.append (s.byte_C(), s.length_i());
}
void
String::operator +=(String s)
{
  append (s);
}

void
String::prepend (String s)
{
  s += *this;
  *this = s;
}

int
String::length_i() const
{
  return strh_.length_i();
}

Byte const*
String::byte_C() const
{
  return strh_.byte_C();
}

char const*
String::ch_C() const
{
  return strh_.ch_C();
}

Byte*
String::byte_l()
{
  return strh_.byte_l();
}

char*
String::ch_l()
{
  return strh_.ch_l();
}

bool 
String::empty_b () const
{
  return !length_i ();
}
/**
  Do a signed comparison,  analogous to memcmp;
 */
int
String::compare_i (String const& s1, String const& s2) 
{
  Byte const* p1 = s1.byte_C();
  Byte const* p2 = s2.byte_C();
  if (p1 == p2)
    return 0;

  /*
    don't forget the terminating '\0'
   */
  int f = (s1.length_i() <? s2.length_i());
  int cmp_length = 1+ f;
  return memcmp (p1, p2, cmp_length);
}


int
String::index_last_i (char const c) const
{
  if (!length_i()) 
    return -1;

  char const* me = strh_.ch_C();
  char const* p = memrchr ((Byte*)me, length_i(), c);
  if (p)
    return p - me;
  return -1;
}

int
String::index_last_i (char const* string) const // UGK!
{
  assert (false);		// broken
  int length = strlen (string); // ugrh
  if (!length_i() || !length) 
    return -1;
  
  int next_i = index_i (string);
  if (next_i == -1)
    return -1;
  
  int index_i = 0;
  while (next_i >= 0) 
    {
      index_i += next_i;
      next_i = right_str (length_i() - index_i - length).index_i (string );
    }
  return index_i;
}

/** find  a character.

  @return
  the index of the leftmost character #c# (0 <= return < length_i()),
  or   -1 if not found. 

  ? should return length_i()?, as in string.left_str (index_i (delimiter))
*/
int
String::index_i (char c) const
{
  char const* me = strh_.ch_C();
  char const* p = (char const *) memchr (me,c,  length_i());
  if (p)
    return p - me;
  return -1;
}

/**
  find a substring.

  @return
1  index of leftmost occurrence of #searchfor#
 */
int
String::index_i (String searchfor) const
{
  char const* me = strh_.ch_C();

  char const* p =     (char const *) 
    memmem (me, length_i(), searchfor.ch_C(), searchfor.length_i ());
  
  if (p)
    return p - me;
  else
    return -1;
}

/** find chars of a set.

  @return
  the index of the leftmost occurance of an element of #set#
  */
int
String::index_any_i (String set) const
{
  int n = length_i();
  if (!n)
    return -1;

  void const * me_l = (void const *) strh_.ch_C();
  for (int i=0; i  < set.length_i(); i++) 
    {
      char * found=(char*) memchr (me_l, set[i], n );
      if (found) 
	{
	  return found - me_l;
	}
    }
  return -1;
}

String
String::left_str (int n) const
{
  if (n >= length_i())
    return *this;

  String retval;    	
  if (n < 1)
    return retval;
  
  retval = *this;
  retval.strh_.trunc (n);
  return retval;
}

String
String::right_str (int n) const
{
  if (n > length_i())
    return *this;
  
  if (n < 1)
    return "";
  
  return String (strh_.byte_C() + length_i() - n, n); 
}


String
String::nomid_str (int index_i, int n) const
{
  if (index_i < 0) 
    {
      n += index_i;
      index_i = 0;
    }
  if (n <= 0)
    return *this;
  
  return
    left_str (index_i)   +
    right_str (length_i() - index_i - n) ;
}

String
String::cut (int index_i, int n) const
{
  if (index_i <0) 
    {
      n += index_i;
      index_i=0;
    }
  
  if (!length_i() || (index_i < 0) || (index_i >= length_i () ) || (n < 1 ) )
    return String();

  if ((n > length_i()) ||  (index_i + n > length_i () ) )
    n = length_i() - index_i;

  return String (byte_C() + index_i, n);
}

String
String::upper_str() const
{
  String str = *this;
  str.to_upper();
  return str;
}
void
String::to_upper()
{
  char *s = (char*)strh_.byte_l();
  strnupr (s ,length_i());
}

void
String::to_lower()
{
  char* s = strh_.ch_l();
  strnlwr (s,length_i());    
}


String 
String::lower_str() const
{
  String str = *this;
  str.to_lower();
  return str;
}
String 
String::reversed_str() const
{
  String str = *this;
  strrev (str.byte_l(), str.length_i ());
  return str;    
}

int
String::value_i() const
{
  return String_convert::dec2_i (*this);
}

double
String::value_f() const
{
  return String_convert::dec2_f (*this);
}

