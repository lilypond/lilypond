/*

  FILE   : string.hh -- implement String inline helper classes,
   and declare stringclass.
 
 
  Rehacked by HWN 3/nov/95
  removed String & 's
  introduced Class String_handle
 */

#ifndef STRING_HH
#define STRING_HH


#include <string.h>
#include <iostream.h>
#include <Rational.h>

#include "stringutil.hh"

/// the smart string class.
/** 

  Intuitive string class. provides 

  ref counting through #String_handle#

  conversion from bool, int, double, char *, char.

  conversion to int, upcase, downcase


  printable. 

  indexing (pos, posAny, lastPos)

  cutting (left, right, mid)

  concat (+=, +)

  signed comparison (<, >, ==, etc)

  No operator[] is provided, since this would be enormously  slow. If needed,
  convert to const char *. 
*/
class String
{
protected:
    String_handle data; // should derive String from String_handle?

public:
    /// init to ""
    /** needed because other constructors are provided.*/
    String() {  }                  
    String(Rational);
    /// String s = "abc";
    String( const char* source ); 
    
    /// "ccccc"
    String( char c, int n = 1 );

    /// String s( 10 );
    String( int i );

    /// 'true' or 'false'
    String(bool );

    /// String s( 3.14, 6, '#' );
    String ( double f , const char *fmt =0);
    String(  int i,  int n,  char c = ' ' );

    ///  return a "new"-ed copy of contents
    char *copy_array() const; //  return a "new"-ed copy of contents

    const char *cptr() const;
    const char *ptr() { return ((const String *)this)->cptr(); }

    /// return the data. Don't use for writing the data.
    operator const char *() const { return cptr(); }
    
    String operator =( const String & source ) { data = source.data; return *this; }

    /// concatenate s
    void operator += (const char *s) { data += s; }
    void operator += (String s);

    char operator []( int n ) const { return data[n]; }

    /// return n leftmost chars
    String left( int n ) const;

    /// return n rightmost chars
    String right( int n ) const;

    /// convert this to upcase
    String upper();

    /// convert this to downcase
    String lower(); // & ??

    /// return the "esrever" of *this
    String reversed() const;


    /// return a piece starting at pos (first char = pos 1), length n
    String mid(int pos,  int n ) const;

    /// cut out a middle piece, return remainder
    String nomid(int pos, int n ) const;

    /// signed comparison,  analogous to strcmp;
    static int compare(const String& s1,const  String& s2);
	
    /// index of rightmost c 
    int lastPos( char c) const;

    /// index of rightmost element of string 
    int lastPos( const char* string ) const;

    /// index of leftmost c
    /**
    RETURN:
    0 if not found, else index + 1
    */
    int pos(char c ) const;
    int pos(const char* string ) const;
    int posAny(const char* string ) const;


    /// provide Stream output
    void printOn(ostream& os) const;

    /// convert to an integer
    long value() const;

    /// convert to a double
    double fvalue() const;
    
    /// the length of the string
    int len() const;

};

#include "compare.hh"

instantiate_compare(const String &, String::compare);

// because const char* also has an operator ==, this is for safety:
inline bool operator==(String s1, const char *s2){
    return s1 == String(s2);
}
inline bool operator==(const char *s1, String s2)
{
    return String(s1)==s2;
}
inline bool operator!=(String s1, const char *s2  ) {
    return s1!=String(s2);
}
inline bool operator!=(const char *s1,String s2) {
    return String(s2) !=s1;
}


inline String
operator  + (String s1, String  s2)
{
    s1 += s2;
    return s1;
}

inline ostream &
operator << ( ostream& os, String d )
{
    d.printOn(os);
    return os;
}


String quoteString(String message, String quote);

#endif
