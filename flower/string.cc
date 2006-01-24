/*
  string.cc - implement String

  (c) 1997--2006 Han-Wen Nienhuys & Jan Nieuwenhuizen
*/

#ifndef _GNU_SOURCE // we want memmem
#define _GNU_SOURCE
#endif

#include "string.hh"

#include <cstdlib>
#include <cstdio>
#include <cassert>
#include <cstring>

#include <iostream>
using namespace std;

#include "libc-extension.hh"
#include "string-convert.hh"

/* std::string conversion helpers */

#if STD_STRING

#include "std-string.hh"

String::String (Std_string const &s)
{
  *this = String (s.c_str ());
}

String::operator Std_string () const
{
  return Std_string (this->c_str ());
}

#endif

/* std::string interface */

String::String (String const &s, int pos, int n)
{
  if (n == -1)
    n = s.length () - pos;
  if (pos == 0)
    *this = s.left_string (n);
  else
    *this = s.right_string (s.length () - pos).left_string (n);
}

String::String (int n, char c)
{
  *this = String_convert::char_string (c, n);
}


bool
String::empty () const
{
  return !length ();
}

int
String::find (char c) const
{
  return index (c);
}

int
String::find (String &s, int pos) const
{
  if (!pos)
    return index (s);
  String f = right_string (length () - pos);
  int n = f.index (s);
  if (n != -1)
    return pos + n;
  return -1;
}

int
String::rfind (char c) const
{
  return index_last (c);
}

String
String::replace (int pos, int n, String str)
{
  return String (*this, 0, pos) + str + String (*this, pos + n);
}



/* String */

#ifdef STRING_DEBUG
void *mymemmove (void *dest, void const *src, size_t n);
#define memmove mymemmove
#endif

// return array, alloced with new.
Byte *
String::get_copy_byte () const
{
  Byte const *src = strh_.to_bytes ();
  Byte *dest = new Byte[strh_.length () + 1];
  memcpy (dest, src, strh_.length () + 1);
  return dest;
}

char *
String::get_copy_str0 () const
{
  return (char *)get_copy_byte ();
}


/*
  copying, constructing.
*/
String &
String::operator = (String const &source)
{
  strh_ = source.strh_;
  return *this;
}

String::String (Byte const *byte, int len_i)
{
  strh_.set (byte, len_i);
}

/**
   @see
   String_convert::
*/
String
to_string (char c, int n)
{
  return String_convert::char_string (c, n);
}

String
to_string (double f, char const *format)
{
  return String_convert::double_string (f, format);
}

String
to_string (int i, char const *format)
{
  return String_convert::int_string (i, format);
}

String
to_string (bool b)
{
  return String_convert::bool_string (b);
}
String
to_string (long b)
{
  return String_convert::long_string (b);
}

String
to_string (char const *format, ...)
{
  va_list args;
  va_start (args, format);
  String str = String_convert::vform_string (format, args);
  va_end (args);
  return str;
}


void
String::append (String s)
{
  strh_.append (s.to_bytes (), s.length ());
}
void
String::operator += (String s)
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
String::length () const
{
  return strh_.length ();
}

Byte const *
String::to_bytes () const
{
  return strh_.to_bytes ();
}

Byte *
String::get_bytes ()
{
  return strh_.get_bytes ();
}

char *
String::get_str0 ()
{
  return strh_.get_str0 ();
}

#ifndef DISALLOW_OLD_STRING
bool
String::is_empty () const
{
  return !length ();
}
#endif


/**
   Do a signed comparison,  analogous to memcmp;
*/
int
String::compare (String const &s1, String const &s2)
{
  Byte const *p1 = s1.to_bytes ();
  Byte const *p2 = s2.to_bytes ();
  if (p1 == p2)
    return 0;

  /*
    don't forget the terminating '\0'
  */
  int f = min (s1.length (), s2.length ());
  int cmp_length = 1+ f;
  int i = memcmp (p1, p2, cmp_length);
  return i;
}


int
String::index_last (char const c) const
{
  if (!length ())
    return -1;

  char const *me = strh_.c_str ();
  char const *p = (char const *)memrchr ((Byte *)me, length (), c);
  if (p)
    return p - me;
  return -1;
}

int
String::index_last (char const *string) const // UGK!
{
  assert (false);		// broken
  int len = strlen (string); // ugrh
  if (!length () || !len)
    return -1;

  int next_i = index (string);
  if (next_i == -1)
    return -1;

  int index_i = 0;
  while (next_i >= 0)
    {
      index_i += next_i;
      next_i = right_string (length () - index_i - len).index (string);
    }
  return index_i;
}

/** find  a character.

@return
the index of the leftmost character #c# (0 <= return < length ()),
or   -1 if not found.

? should return length ()?, as in string.left_string (index (delimiter))
*/
int
String::index (char c) const
{
  char const *me = strh_.c_str ();
  char const *p = (char const *) memchr (me, c, length ());
  if (p)
    return p - me;
  return -1;
}

/**
   find a substring.

   @return
   index of leftmost occurrence of #searchfor#
*/
int
String::index (String searchfor) const
{
  char const *me = strh_.c_str ();

  char const *p
    = (char const *) memmem (me, length (),
			     searchfor.c_str (), searchfor.length ());

  if (p)
    return p - me;

  return -1;
}

/** find chars of a set.

@return

the index of the leftmost occurance of an element of #set#.  -1 if
nothing is found.
*/
int
String::index_any (String set) const
{
  int n = length ();
  if (!n)
    return -1;

  void const *me = (void const *) strh_.c_str ();
  for (int i = 0; i < set.length (); i++)
    {
      char *found = (char *) memchr (me, set[i], n);
      if (found)
	return found - (char const *)me;
    }
  return -1;
}

String
String::left_string (int n) const
{
  if (n >= length ())
    return *this;

  String retval;
  if (n < 1)
    return retval;

  retval = *this;
  retval.strh_.trunc (n);
  return retval;
}

String
String::right_string (int n) const
{
  if (n > length ())
    return *this;

  if (n < 1)
    return "";

  return String (strh_.to_bytes () + length () - n, n);
}

String
String::nomid_string (int index_i, int n) const
{
  if (index_i < 0)
    {
      n += index_i;
      index_i = 0;
    }
  if (n <= 0)
    return *this;

  return left_string (index_i) + right_string (length () - index_i - n);
}

String
String::cut_string (int index_i, int n) const
{
  if (index_i < 0)
    {
      n += index_i;
      index_i = 0;
    }

  if (!length () || (index_i < 0) || (index_i >= length ()) || (n < 1))
    return String ();

  if ((n > length ()) || (index_i + n > length ()))
    n = length () - index_i;

  return String (to_bytes () + index_i, n);
}


void
String::to_upper ()
{
  strnupr (get_str0 (), length ());
}

void
String::to_lower ()
{
  strnlwr (get_str0 (), length ());
}

void
String::reverse ()
{
  memrev (get_bytes (), length ());
}

int
String::to_int () const
{
  return String_convert::dec2int (*this);
}

double
String::to_double () const
{
  return String_convert::dec2double (*this);
}

#ifdef STREAM_SUPPORT
ostream &
operator << (ostream &os, String d)
{
  d.print_on (os);
  return os;
}

void
String::print_on (ostream &os) const
{
  if (!strh_.is_binary_bo ())
    os << c_str ();
  else
    for (int i = 0; i < length (); i++)
      os << (Byte) (*this)[ i ];
}
#endif

String
String::substitute (String find, String replace)
{
  int n = find.length ();
  int m = replace.length ();
  for (int i = index (find), j = 0; i > -1;
       i = right_string (length () - j).index (find))
    {
      *this = left_string (i + j)
	+ replace
	+ right_string (length () - j - i - n);
      j += i + m;
    }
  return *this;
}

String
String::substitute (char find, char replace)
{
  for (int i = index (find); i > - 1; i = index (find))
    (*this)[i] = replace;
  return *this;
}
