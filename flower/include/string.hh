/*
  FILE   : string.hh -- declare String

  Rehacked by HWN 3/nov/95
  removed String & 's
  introduced Class String_handle
*/

#ifndef STD_STRING_HH
#error string.hh is obsolete, use std-string.hh
#endif

#ifndef STRING_HH
#define STRING_HH

// too darn slow with gcc3
#ifdef STREAM_SUPPORT
#if (__GNUC__ > 2)
#include <iostream>
#else
class ostream;
#endif
#endif

#include "arithmetic-operator.hh"
#include "string-handle.hh"

namespace std {

class String
{
public:

  /* partial std::string interface */
  String ();
  String (int n, char c);
  String (char const *source);
  String (char const *, int n);
  String (String const &, int pos, ssize n=NPOS);

  String &operator = (String const &source);
  /// concatenate s
  void operator += (char const *s) { strh_ += s; }
  void operator += (String s);
  char &operator [] (int n);
  char operator [] (int n) const;


  char const *c_str () const;
  char const *data () const;
  bool empty () const;
  int find (String s, int pos=0) const;
  int find (char c, int pos=0) const;
  int find (char const *c, int pos=0) const;
  int rfind (char c) const;
  String replace (int pos, int n, String str);

  String substr (int pos=0, ssize n=NPOS) const;
  int compare (String const &s) const;

  void append (String);
  int length () const;

  String insert (ssize pos, String);
  ssize copy (char *s, ssize n, ssize pos=0) const;

protected:
  String_handle strh_;

  bool null_terminated ();

private:
  ///  return "new"-ed copy of contents
  Byte *get_copy_byte () const;
  char *get_copy_str0 () const;

  Byte const *to_bytes () const;
  Byte *get_bytes ();

  void prepend (String);

  /// return n leftmost chars
  String left_string (int n) const;

  /// return n rightmost chars
  String right_string (int n) const;

  /// return a piece starting at index (first char = index_i 0), length n
  String cut_string (int index_i, int n) const;

  /// cut out a middle piece, return remainder
  String nomid_string (int index_i, int n) const;

  static int compare (String const &s1, const String &s2);

  /// index of rightmost character C in string
  int index_last (char c) const;
  
  int index (char c) const;

  /// index of leftmost occurance of STRING
  int index (String) const;

  int index_any (String) const;

#ifdef STREAM_SUPPORT
  /// provide Stream output
  void print_on (ostream &os) const;
#endif

  /// convert to an integer
  int to_int () const;

  /// convert to a double
  double to_double () const;

  String substitute (String find, String replace);
  String substitute (char find, char replace);
};

#ifdef STRING_UTILS_INLINED
#ifndef INLINE
#define INLINE inline
#endif
#include "string.icc"
/* we should be resetting INLINE. oh well. */
#endif

// because char const* also has an operator ==, this is for safety:
bool operator == (String s1, char const *s2);
bool operator == (char const *s1, String s2);
bool operator != (String s1, char const *s2);
bool operator != (char const *s1, String s2);

IMPLEMENT_ARITHMETIC_OPERATOR (String, +);
#ifdef STREAM_SUPPORT
ostream &operator << (ostream &os, String d);
#endif

}

/*
  technically incorrect, but lets keep it here: this is a
  catch all place for this stuff.
*/
#include "international.hh"


#endif /* STRING_HH */
