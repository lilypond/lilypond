/*
  PROJECT: FlowerSoft C++ library
  FILE   : string.cc

  Rehacked by HWN 3/nov/95
  removed String &
  introduced Class String_handle
  */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>

#include "string.hh"


static char* 
strlwr( char* s )
{
    char* p = s;

    while( *p )
        {
        *p = tolower( *p );    /* a macro on some compilers */
        p++;
        }
    return s;
}

static char* 
strupr( char* s )
{
    char* p = s;

    while( *p )
        {
        *p = toupper( *p );    /* a macro on some compilers */
        p++;
        }
    return s;
}

String::String(Rational r)
{
    char * n = Itoa(r.numerator()); // LEAK????
    
    *this = n;
    if (r.denominator() != 1) {
	char * d = Itoa(r.denominator());
	*this +=  String( '/' ) + String(d);
	//delete d;
    }
/*    delete n;
    */
}

// return array, alloced with new.
Byte*
String::copy_by_p() const
{
    Byte const* src = strh_.by_c_l();
    Byte* dest = new Byte[strh_.length_i() + 1];
    memmove( dest, src, strh_.length_i() + 1 );
    return dest;    
}

void
String::printOn(ostream& os) const
{
    if ( length_i() == strlen( ch_c_l() ) )
        os << ch_c_l();
    else
	for ( int i = 0; i < length_i(); i++ )
	    os << (Byte)(*this)[ i ];
}

String::String (bool b)
{
    *this = (char const* ) (b ? "true" : "false");
}
String::String( char const* source )
{   
    assert(source);    
    strh_ = source;    
}

String::String( Byte const* by_l, int length_i )
{   
    assert( !length_i || by_l );
    strh_.set( by_l, length_i );    
}

void
String::operator +=(String s)
{
    strh_.append( s.by_c_l(), s.length_i() );
}

int
String::length_i() const
{
    return strh_.length_i();
}

String::String(char c,  int n)
{
    int l = n;
    assert(n >= 0 && n <= 80); // what the fuck is 80?
//min(max( n, 0 ), 80); 
    char s[81];
    memset(s, c, l);
    s[l] = 0;
//    strh_ = s;
    strh_.set( (Byte*)s, n );
}

String::String(int i)
{
    char digits[ 81 ];             // who the fuck is 80???
    digits[ 0 ] = '\0';
    sprintf(digits, "%d", i );     // assume radix 10
    strh_ = digits;
}

String::String( const int i, const int n, char const c )
{
    char fillChar = c;
    if ( fillChar)
        fillChar = '0';

    String v( i );
    
    String str = String( fillChar, n - v.length_i() ) + String( v );
    strh_.set( str.by_c_l(), str.length_i() );
}

Byte const*
String::by_c_l() const
{
    return strh_.by_c_l();
}

char const*
String::ch_c_l() const
{
    return strh_.ch_c_l();
}

Byte*
String::by_l()
{
    return strh_.by_l();
}

char*
String::ch_l()
{
    return strh_.ch_l();
}

// signed comparison,  analogous to memcmp;
int
String::compare(String const& s1, String const& s2 ) 
{
    Byte const* p1 = s1.by_c_l();
    Byte const* p2 = s2.by_c_l();
    if ( p1 == p2 )
	return 0;

    int i1 = s1.length_i();
    int i2 = s2.length_i();
    int i = i1 <? i2;

    if (! i ) 
	return 0;
	
    int result =  memcmp( p1, p2, i );
    
    return (result)? result : i1 - i2;
}


int
String::lastPos( char const c ) const
{
    // not binary safe
    assert( length_i() == strlen( ch_c_l() ) );
    char const* me = strh_.ch_c_l();
    int pos = 0;
    if ( length_i() )
        {
	char const* p = strrchr(me, c );
        if ( p )
            pos = p - me + 1;
        }
    return pos;
}

int
String::lastPos( char const* string ) const
{
    // not binary safe
    assert( length_i() == strlen( ch_c_l() ) );
    int pos = 0;
    int length = strlen( string );
    if ( length_i() && length )
        {
        int nextpos = this->pos( string );
        while( nextpos )
            {
            pos += nextpos;
            nextpos = right( length_i() - pos - length + 1 ).pos( string );
            }
        }
    return pos;
}

// find c
// return 0 if not found. 

// ? should return length_i()?, as in string.left(pos(delimiter))
int
String::pos(char c ) const
{
    // not binary safe
    assert( length_i() == strlen( ch_c_l() ) );
    char const* me = strh_.ch_c_l();
    int pos = 0;
    if ( length_i() )
        {
	char const* p = strchr( me, c );
        if ( p )
            pos = p - me + 1;
        }
    return pos;
}

// find searchfor. (what if this == "" && searchfor == "") ???
int
String::pos( char const* searchfor ) const
{
    // not binary safe
    assert( length_i() == strlen( ch_c_l() ) );
    char const* me = strh_.ch_c_l();
    int pos = 0;
    if ( length_i() && searchfor)
        {
	char const* p = strstr(me, searchfor);
        if ( p )
	    pos = p - me + 1;
        }
    return pos;
}

// find chars of a set.
int
String::posAny( char const* string ) const
{
    // not binary safe
    assert( length_i() == strlen( ch_c_l() ) );
    int pos = 0;
    char const* s = (char const* )strh_.ch_c_l();
    if ( length_i() && string )
        {
	char const* p = strpbrk( s, string );
        if ( p )
	    pos = p - s + 1;
        }
    return pos;
}

String
String::left( int n ) const
{
    if (n >= length_i())
	return *this;

    String retval;    	
    if (n < 1)
        return retval;
    
    retval = *this;
    retval.strh_.trunc(n);
    return retval;
}


// n rightmst chars
String
String::right( int n ) const
{
    if (n > length_i())
	return *this;
    
    if ( n < 1)
        String(); 
    
    return String( strh_.by_c_l() + length_i() - n, n ); 
}


String
String::nomid( const int pos, const int n ) const
{
    String retval;
        
    if ( pos < 1 )
        return String("");
    if ( pos > length_i())
	return *this;
    
    return String( String( left( pos - 1 ) ) + right( length_i() - pos - n + 1 ));
}


String
String::mid( int pos, int n ) const
{
    // HWN. This SUX: JCN: yep, please change me + all my invocations
    // pos 1 == strh_->string[ 0 ];
    // pos 0 allowed for convenience
    if ( !length_i() || ( pos < 0 ) || ( pos > length_i() ) && ( n < 1 ) )
        return String();

    // overflow...
    if ( ( n > length_i() ) ||  ( pos + n - 1 > length_i() ) )
	n = length_i() - pos + 1;

    return String( by_c_l() + pos -1, n );
}


// to  uppercase
String
String::upper()
{
    // not binary safe
    assert( length_i() == strlen( ch_c_l() ) );
    char *s = strh_.by_l();
    strupr( s );
    return *this;
}


// to lowercase
String 
String::lower()
{
    // not binary safe
    assert( length_i() == strlen( ch_c_l() ) );
    char* s = strh_.by_l();
    strlwr(s);
    return *this;
}

String::String (double f, char const* fmt)
{
    /* worst case would be printing HUGE (or 1/HUGE), which is approx
       2e318, this number would have approx 318 zero's in its string.

      1024 is a safe length for the buffer
      */

    char buf[1024]; 
    if (!fmt)
	sprintf(buf, "%f", f);
    else
	sprintf(buf, fmt,f);
    *this = buf;
}

long
String::value() const
{
    long l =0;
    if (length_i()) {
	int conv = sscanf(strh_.ch_c_l(), "%ld", &l);
	assert(conv);
    }
    return l;
}

double
String::fvalue() const
{
    double d =0;
    if (length_i()) {
	int conv = sscanf(strh_.ch_c_l(), "%lf", &d);
	assert(conv);
    }
    return d;
}


String quoteString( String msg, String quote)
{
    return msg + " `" + quote  + "' ";
}


Byte*
strrev( Byte* by_l, int length_i )
{
  Byte by;
  Byte* left_by_l = by_l;
  Byte* right_by_l = by_l + length_i;

  while ( right_by_l > left_by_l ) {
    by = *left_by_l;
    *left_by_l++ = *right_by_l;
    *right_by_l-- = by;
  }
  return by_l;
}


String 
String::reversed() const
{
    String str = *this;
    strrev( str.by_l(), str.length_i() );
    return str;    
}
