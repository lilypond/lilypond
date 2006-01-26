/*
  string.cc - implement String

  (c) 1997--2006 Han-Wen Nienhuys & Jan Nieuwenhuizen
*/
#if !STD_STRING

#ifndef _GNU_SOURCE // we want memmem
#define _GNU_SOURCE
#endif

#include "std-string.hh"

#include <cstdlib>
#include <cstdio>
#include <cassert>
#include <cstring>

using namespace std;

#include "libc-extension.hh"
#include "string-convert.hh"

/* std::string interface */

namespace std {

String::String (char const *s, int n)
{
  strh_.set ((Byte const *)s, n);
}

String::String (String const &s, int pos, ssize n)
{
  *this = s.substr (pos, n);
}

String::String (int n, char c)
{
  *this = String_convert::char_string (c, n);
}

String &
String::operator = (String const &source)
{
  strh_ = source.strh_;
  return *this;
}

String
String::substr (int pos, ssize n) const
{
#if 1
  if (n == (ssize)-1 || n == (ssize)INT_MAX || n == NPOS)
    n = length () - pos;
  return cut_string (pos, n);
#else
  if (n == (ssize)-1 || n == (ssize)INT_MAX || n == NPOS)
    n = length () - pos;
  if (pos == 0)
    return left_string (n);
  else
    return right_string (length () - pos).left_string (n);
#endif
}

String
String::insert (ssize pos, String s)
{
  *this = substr (0, pos) + s + substr (pos + 1);
  return *this;
}

ssize
String::copy (char *buf, ssize n, ssize pos) const
{
  assert (pos == 0);
  memcpy (buf, strh_.to_bytes (), strh_.length () + 1);
  return n; // ?
}

int
String::compare (String const &s) const
{
  char const *p1 = c_str ();
  char const *p2 = s.c_str ();
  if (p1 == p2)
    return 0;

  /*
    don't forget the terminating '\0'
  */
  int f = min (length (), s.length ());
  int cmp_length = 1+ f;
  int i = memcmp (p1, p2, cmp_length);
  return i;
}

char const *
String::data () const
{
  return (char const*) to_bytes ();
}

bool
String::empty () const
{
  return !length ();
}

int
String::find (char c, int pos) const
{
  String f = right_string (length () - pos);
  ssize n = f.index (c);
  if (n != NPOS)
    return pos + n;
  return NPOS;
}

int
String::find (char const *c, int pos) const
{
  return find (String (c), pos);
}

int
String::find (String s, int pos) const
{
  if (!pos)
    return index (s);
  String f = right_string (length () - pos);
  ssize n = f.index (s);
  if (n != NPOS)
    return pos + n;
  return NPOS;
}

int
String::rfind (char c) const
{
  return index_last (c);
}

String
String::replace (int pos, int n, String str)
{
  return this->substr (0, pos) + str + this->substr (pos + n);
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

int
String::length () const
{
  return strh_.length ();
}




/* String */

int
String::compare (String const &s1, String const &s2)
{
  return s1.compare (s2);
}

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



#if 0
void
String::prepend (String s)
{
  s += *this;
  *this = s;
}

#endif


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


int
String::index_last (char const c) const
{
  if (!length ())
    return NPOS;

  char const *me = strh_.c_str ();
  char const *p = (char const *)memrchr ((Byte *)me, length (), c);
  if (p)
    return p - me;
  return NPOS;
}

/** find  a character.

@return
the index of the leftmost character #c# (0 <= return < length ()),
or   NPOS if not found.

? should return length ()?, as in string.left_string (index (delimiter))
*/
int
String::index (char c) const
{
  char const *me = strh_.c_str ();
  char const *p = (char const *) memchr (me, c, length ());
  if (p)
    return p - me;
  return NPOS;
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

  return NPOS;
}

/** find chars of a set.

@return

the index of the leftmost occurance of an element of #set#.  NPOS if
nothing is found.
*/
int
String::index_any (String set) const
{
  int n = length ();
  if (!n)
    return NPOS;

  void const *me = (void const *) strh_.c_str ();
  for (int i = 0; i < set.length (); i++)
    {
      char *found = (char *) memchr (me, set[i], n);
      if (found)
	return found - (char const *)me;
    }
  return NPOS;
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

  return String (strh_.c_str () + length () - n, n);
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

  return String (c_str () + index_i, n);
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
#include <iostream>

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
  for (ssize i = index (find), j = 0; i != NPOS;
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
  for (ssize i = index (find); i != NPOS; i = index (find))
    (*this)[i] = replace;
  return *this;
}

}

#endif /* !STD_STRING */

