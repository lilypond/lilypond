/*

 string.cc - implement String
 
 (c) 1997 Han-Wen Nienhuys & Jan Nieuwenhuizen

 */

#include <stdlib.h>
#include <stdio.h>

#include <assert.h>
#include <string.h>

#include "string.hh"
#include "libc-extension.hh"
#include "string-convert.hh"

#ifdef STRING_DEBUG
void* mymemmove( void* dest, void const* src, size_t n );
#define memmove mymemmove
#endif

// return array, alloced with new.
Byte*
String::copy_byte_p() const
{
    Byte const* src = strh_.byte_c_l();
    Byte* dest = new Byte[strh_.length_i() + 1];
    memcpy( dest, src, strh_.length_i() + 1 );
    return dest;    
}
void
String::print_on(ostream& os) const
{
    if (strh_.null_terminated_b())
        os << ch_c_l();
    else
	for ( int i = 0; i < length_i(); i++ )
	    os << (Byte)(*this)[ i ];
}

/*
  copying, constructing.
 */
String&
String::operator = (String const&source )
{
    strh_ = source.strh_;
    return *this;
}


String::String(Rational r)
{
    *this = String_convert::rational_str(r);
}

String::String (double f, char const* fmt)
{
    *this= String_convert::double_str(f,fmt);
}

String::String( char c,  int n )
{
    *this = String_convert::char_str (c,n);
}

/**
  @see
  String_convert::int_str
 */
String::String(int i, const char * format )
{
    *this = String_convert::int_str(i,format);
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
    strh_.set( byte_l, length_i );    
}

void
String::append(String s)
{
    strh_.append( s.byte_c_l(), s.length_i() );
}
void
String::operator +=(String s)
{
    append(s);
}

void
String::prepend(String s)
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

/**
  Do a signed comparison,  analogous to memcmp;
 */
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
    if ( !length_i() ) 
	return -1;

    char const* me = strh_.ch_c_l();
    char const* p = memrchr(me, length_i(), c );
    if ( p )
	return p - me;
    return -1;
}

int
String::index_last_i( char const* string ) const // UGK!
{
    assert(false);		// broken
    int length = strlen( string ); // ugrh
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

/** find  a character.

  @return
  the index of the leftmost character #c# (0 <= return < length_i()),
  or   -1 if not found. 

  ? should return length_i()?, as in string.left_str(index_i(delimiter))
*/
int
String::index_i(char c ) const
{
    char const* me = strh_.ch_c_l();
    char const* p = (char const *) memchr( me,c,  length_i());
    if ( p )
	return p - me;
    return -1;
}

/**
  find the substring.

  @return
  index of leftmost occurrence of #searchfor#
 */
int
String::index_i( String searchfor ) const
{
    char const* me = strh_.ch_c_l();
    char const* p = (char const *) memmem(me, length_i(), searchfor.ch_c_l(), 
					  searchfor.length_i());
    
    if ( p )
	return p - me;
    else
	return -1;
}

/** find chars of a set.

  @return
  the index of the leftmost occurance of an element of #set#
  */
int
String::index_any_i( String set ) const
{
    int n = length_i();
    if ( !n )
	return -1;

    const void * me_l = (const void*) strh_.ch_c_l();
    for (int i=0; i  < set.length_i(); i++) {
	char * found=(char*) memchr(me_l, set[i], n  );
	if (found) {
	    return found - me_l;
	}
    }
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

String
String::right_str( int n ) const
{
    if (n > length_i())
	return *this;
    
    if ( n < 1)
        return "";
    
    return String( strh_.byte_c_l() + length_i() - n, n ); 
}


String
String::nomid_str( int index_i, int n ) const
{
    if ( index_i < 0 ) {
	n += index_i;
	index_i = 0;
    }
    if ( n <= 0)
	return *this;
    
    return
	left_str( index_i )   +
	right_str( length_i() - index_i - n ) ;
}

/*
  proposal: change to "cut()"
 */
String
String::mid_str( int index_i, int n ) const
{
    if ( !length_i() || ( index_i < 0 ) || ( index_i >= length_i() ) || ( n < 1 ) )
	return String();

    if ( ( n > length_i() ) ||  ( index_i + n > length_i() ) )
	n = length_i() - index_i;

    return String( byte_c_l() + index_i, n );
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
    strnupr( s ,length_i());
}

void
String::to_lower()
{
    char* s = strh_.ch_l();
    strnlwr(s,length_i());    
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
    strrev( str.byte_l(), str.length_i() );
    return str;    
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


