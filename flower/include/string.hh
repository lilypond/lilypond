/*

  FILE   : string.hh -- declare String
 
  Rehacked by HWN 3/nov/95
  removed String & 's
  introduced Class String_handle
 */

#ifndef STRING_HH
#define STRING_HH


// too darn slow with gcc3
#ifdef STREAM_SUPPORT
#if ( __GNUC__ > 2 )
#include <iostream.h>
#else
class ostream;
#endif
#endif

#include "arithmetic-operator.hh"
#include "flower-proto.hh"
#include "string-handle.hh"

/** 
 
  Intuitive string class. provides 
\begin{itemize}
\item
  ref counting through #String_handle#
\item
  conversion from bool, int, double, char* , char.  
\item
  to be moved to String_convert:
  conversion to int, upcase, downcase 

\item
  printable. 

\item
  indexing (index_i, index_any_i, last_index_i)

\item
  cutting (left_string, right_string, mid_string)

\item
  concat (+=, +)

\item
  signed comparison (<, >, ==, etc)

\item
  No operator[] is provided, since this would be enormously  slow. If needed,
  convert to char const* .
\end{itemize}

*/
class String
{
protected:
  String_handle strh_; 

  bool null_terminated ();
    
public:

  /** init to empty string. This is needed because other
    constructors are provided.*/
  String ();

  /// String s = "abc";
  String (char const* source); 
  String (Byte const* byte, int length_i); 
    
  ///  return "new"-ed copy of contents
  Byte* get_copy_byte () const;
  char* get_copy_str0 () const;

  char const* to_str0 () const;
  Byte const* to_bytes () const;
  char* get_str0 ();
  Byte* get_bytes ();

  String &operator = (String const & source);

  /// concatenate s
  void operator += (char const* s) { strh_ += s; }
  void operator += (String s);

  bool empty_b () const;

  void append (String);
  void prepend (String);

  /**
    Return a char.  UNSAFE because it may change strlen () result
   */
  char &operator [] (int n);
  char operator [] (int n) const;

  /// return n leftmost chars
  String left_string (int n) const;

  /// return n rightmost chars
  String right_string (int n) const;

  /// return uppercase of *this
  String upper_string () const;

  /// return lowercase of *this
  String lower_string () const;

  /// return the "esrever" of *this
  String reversed_string () const;

  /// return a piece starting at index (first char = index_i 0), length n
  String cut_string (int index_i, int n) const;

  /// cut out a middle piece, return remainder
  String nomid_string (int index_i, int n) const;

  /// signed comparison,  analogous to memcmp;
  static int compare (String const & s1,const  String& s2);
	
  /// index of rightmost c 
  int index_last (char c) const;

  /// index of rightmost element of string (???)
  int index_last (char const* string) const;

  int index (char c) const;

  /// index of leftmost occurance of STRING
  int index (String) const;


  int index_any (String) const;

  void to_upper ();
  void to_lower ();

#ifdef STREAM_SUPPORT
  /// provide Stream output
  void print_on (ostream& os) const;
#endif
  
  /// the length of the string
  int length () const;

  /// convert to an integer
  int to_int () const;

  /// convert to a double
  double to_double () const;
};

/*
 better to clutter global namespace, than suffer *ugh, ugh, ugh*
 implicit conversions.

 it might be cool to have no type-checking at all in a language,
 but once there is, having this silently circumvented is a nightmare.

 whenever implicit conversions seem necessary (e.g. operator << ()),
 use Scalar as the generic type iso String.
 */

/// for completeness (=handy)
inline String to_string (String s) { return s; }
/// "cccc"
String to_string (char c, int n = 1);
String to_string (int i, char const* format = 0);
String to_string (double f , char const* format = 0);
String to_string (long  b);
String to_string (bool b);
String to_string (char const* format, ... );

/*
  technically incorrect, but lets keep it here: this is a
  catch all place for this stuff.
  */
  
#include "international.hh"


#include "compare.hh"
INSTANTIATE_COMPARE (String const &, String::compare);

#ifdef STRING_UTILS_INLINED
#ifndef INLINE
#define INLINE inline
#endif
#include "string.icc"
/* we should be resetting INLINE. oh well. */
#endif


// because char const* also has an operator ==, this is for safety:
inline bool operator== (String s1, char const* s2)
{
  return s1 == String (s2);
}
inline bool operator== (char const* s1, String s2)
{
  return String (s1)==s2;
}
inline bool operator!= (String s1, char const* s2 ) {
  return s1!=String (s2);
}
inline bool operator!= (char const* s1,String s2) {
  return String (s2) !=s1;
}

IMPLEMENT_ARITHMETIC_OPERATOR (String, +);
#ifdef STREAM_SUPPORT
ostream &operator << (ostream& os, String d);
#endif

#endif
