/*
  PROJECT: FlowerSoft C++ library
  FILE   : string.cc

  Rehacked by HWN 3/nov/95
  removed String &
  introduced class String_handle
  */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>

#include "string.hh"

#ifdef STRING_DEBUG
void* mymemmove( void* dest, void const* src, size_t n );
#define memmove mymemmove
#endif

static char* 
strlwr( char* s )
{
    char* p = s;

    while( *p ) {
        *p = tolower( *p );    /* a macro on some compilers */
        p++;
    }
    return s;
}

static char* 
strupr( char* s )
{
    char* p = s;

    while( *p ) {
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
String::copy_byte_p() const
{
    Byte const* src = strh_.byte_c_l();
    Byte* dest = new Byte[strh_.length_i() + 1];
    memmove( dest, src, strh_.length_i() + 1 );
    return dest;    
}

void
String::print_on(ostream& os) const
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

String::String( Byte const* byte_l, int length_i )
{   
//    assert( !length_i || byte_l );// ugh.  Storing null pointers? 
    strh_.set( byte_l, length_i );    
}

void
String::operator +=(String s)
{
    strh_.append( s.byte_c_l(), s.length_i() );
}

int
String::length_i() const
{
    return strh_.length_i();
}

// will go away, fixed anyway
String::String( char c,  int n )
{
    n = n >= 0 ? n : 0;
    char* ch_p = new char[ n ];
    memset( ch_p, c, n );
    strh_.set( (Byte*)ch_p, n );
    delete ch_p;
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
    char fill_ch = c;
    if ( fill_ch)
        fill_ch = '0';

    String v( i );
    
    String str = String( fill_ch, n - v.length_i() ) + String( v );
    strh_.set( str.byte_c_l(), str.length_i() );
}

Byte const*
String::byte_c_l() const
{
    return strh_.byte_c_l();
}

char const*
String::ch_c_l() const
{
    return strh_.ch_c_l();
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

// signed comparison,  analogous to memcmp;
int
String::compare_i(String const& s1, String const& s2 ) 
{
    Byte const* p1 = s1.byte_c_l();
    Byte const* p2 = s2.byte_c_l();
    if ( p1 == p2 )
	return 0;

    int i1 = s1.length_i();
    int i2 = s2.length_i();
    int i = i1 <? i2;

    int result=  memcmp( p1, p2, i );
    return result ? result : i1-i2;
}


int
String::index_last_i( char const c ) const
{
	// not binary safe
	assert( length_i() == strlen( ch_c_l() ) );
	if ( !length_i() ) 
		return -1;

	char const* me = strh_.ch_c_l();
	char const* p = strrchr(me, c );
	if ( p )
		return p - me;
	return -1;
}

int
String::index_last_i( char const* string ) const
{
	// not binary safe
	assert( length_i() == strlen( ch_c_l() ) );

	int length = strlen( string );
	if ( !length_i() || !length ) 
		return -1;

	int next_i = index_i( string );
	if ( next_i == -1 )
		return -1;
		
	int index_i = 0;
	while( next_i >= 0 ) {
		index_i += next_i;
		next_i = right_str( length_i() - index_i - length ).index_i( string );
	}
	return index_i;
}

// find c
// return -1 if not found. 

// ? should return length_i()?, as in string.left_str(index_i(delimiter))
int
String::index_i(char c ) const
{
	// not binary safe
	assert( length_i() == strlen( ch_c_l() ) );
	if ( !length_i() )
		return -1;

	char const* me = strh_.ch_c_l();
	char const* p = strchr( me, c );
	if ( p )
		return p - me;
	return -1;
}

// find searchfor. (what if this == "" && searchfor == "") ???
int
String::index_i( char const* searchfor ) const
{
	// not binary safe
	assert( length_i() == strlen( ch_c_l() ) );
	if ( !length_i() || !searchfor )
		return -1;
		
	char const* me = strh_.ch_c_l();
	char const* p = strstr(me, searchfor);
	if ( p )
		return p - me;
	return -1;
}

// find chars of a set.
int
String::index_any_i( char const* string ) const
{
	// not binary safe
	assert( length_i() == strlen( ch_c_l() ) );

	if ( !length_i() || !string )
		return -1;
		
	char const* s = (char const* )strh_.ch_c_l();
	char const* p = strpbrk( s, string );
	if ( p )
		return p - s;
	return -1;
}

String
String::left_str( int n ) const
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
String::right_str( int n ) const
{
    if (n > length_i())
	return *this;
    
    if ( n < 1)
        String(); 
    
    return String( strh_.byte_c_l() + length_i() - n, n ); 
}


String
String::nomid_str( int index_i, int n ) const
{
	if ( index_i < 0 )
		return String();
	if ( index_i >= length_i() )
		return *this;
    
	return String( String( left_str( index_i ) ) + right_str( length_i() - index_i - n ));
}


String
String::mid_str( int index_i, int n ) const
{
	if ( !length_i() || ( index_i < 0 ) || ( index_i >= length_i() ) || ( n < 1 ) )
		return String();

	if ( ( n > length_i() ) ||  ( index_i + n > length_i() ) )
		n = length_i() - index_i;

	return String( byte_c_l() + index_i, n );
}


// return uppercase
String
String::upper_str() const
{
    // not binary safe
    assert( length_i() == strlen( ch_c_l() ) );
    String str = *this;
    char *s = str.strh_.byte_l();
    strupr( s );
    return str;
}


// return lowercase
String 
String::lower_str() const
{
    // not binary safe
    assert( length_i() == strlen( ch_c_l() ) );
    String str = *this;
    char* s = str.strh_.ch_l();
    strlwr(s);
    return str;
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

int
String::value_i() const
{
	return String_convert::dec2_i( *this );
}

double
String::value_f() const
{
	return String_convert::dec2_f( *this );
}


#if 0
String 
quoteString( String msg, String quote)
{
     return msg + " `" + quote  + "' ";
}
#endif // 0

Byte*
strrev( Byte* byte_l, int length_i )
{
  Byte byte;
  Byte* left_byte_l = byte_l;
  Byte* right_byte_l = byte_l + length_i;

  while ( right_byte_l > left_byte_l ) {
    byte = *left_byte_l;
    *left_byte_l++ = *right_byte_l;
    *right_byte_l-- = byte;
  }
  return byte_l;
}


String 
String::reversed_str() const
{
    String str = *this;
    strrev( str.byte_l(), str.length_i() );
    return str;    
}
