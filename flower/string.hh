/*

  FILE   : string.hh -- declare String
 
  Rehacked by HWN 3/nov/95
  removed String & 's
  introduced Class String_handle
 */

#ifndef STRING_HH
#define STRING_HH


#include <string.h>
#include <iostream.h>
#include <Rational.h>

#include "stringhandle.hh"

/** 
 
  Intuitive string class. provides 
\begin{itemize}
\item
  ref counting through #String_handle#
\item

  conversion from bool, int, double, char *, char.
\item

  conversion to int, upcase, downcase

\item

  printable. 

\item
  indexing (pos, posAny, lastPos)

\item
  cutting (left, right, mid)

\item
  concat (+=, +)

\item
  signed comparison (<, >, ==, etc)

\item
  No operator[] is provided, since this would be enormously  slow. If needed,
  convert to const char *.
\end{itemize}
*/
class String
{
protected:
    String_handle strh_; // should derive String from String_handle?

public:

    /**   init to "". needed because other constructors are provided.*/
    String() {  }                  
    String(Rational);
    /// String s = "abc";
    String( const char* source ); 

    String( Byte const* l_byte_c, int length_i ); 
    
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
    Byte* copy_byte_p() const; //  return a "new"-ed copy of contents

    char const* ch_c_l() const;
    Byte const* byte_c_l() const;
    char* ch_l();
    Byte* byte_l();

    /// deprecated; use ch_c_l()
    operator const char *() const { return ch_c_l(); }
    
    String operator =( const String & source ) { strh_ = source.strh_; return *this; }

    /// concatenate s
    void operator += (char const* s) { strh_ += s; }
    void operator += (String s);

    char operator []( int n ) const { return strh_[n]; }

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


    /// return a piece starting at pos (first char = pos 1), ength n
    String mid(int pos,  int n ) const;

    /// cut out a middle piece, return remainder
    String nomid(int pos, int n ) const;

    /// signed comparison,  analogous to memcmp;
    static int compare(const String& s1,const  String& s2);
	
    /// index of rightmost c 
    int lastPos( char c) const;

    /// index of rightmost element of string 
    int lastPos( const char* string ) const;

    /**
      index of leftmost c.
      
    @return
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
    int length_i() const;

    // deprecated 
    int len() const {
    	return length_i();
    }

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

#include "stringconversion.hh"

#endif
